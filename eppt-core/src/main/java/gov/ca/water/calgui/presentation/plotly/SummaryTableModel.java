/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.presentation.plotly;

import java.time.Month;
import java.time.YearMonth;
import java.time.format.TextStyle;
import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputed;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedStatistics;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;

import com.rma.javafx.treetable.RmaTreeTableModel;
import com.rma.javafx.treetable.columns.specs.RmaTreeTableColumnSpec;
import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;

import static java.util.stream.Collectors.toSet;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
class SummaryTableModel extends RmaTreeTableModel<SummaryPaneRow>
{
	private final RmaTreeTableColumnSpec _rootColSpec;
	private final RmaTreeTableColumnSpec _aggregateColumnSpec;

	SummaryTableModel(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet epptReportingComputedSet,
					  List<MonthPeriod> selectedMonthlyPeriods)
	{
		_rootColSpec = new RmaTreeTableColumnSpec.Builder(plotTitle + " (" + epptReportingComputedSet.getUnits() + ")")
				.withCanBeHidden(false)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
		MonthPeriod monthPeriod = epptReportingComputedSet.getEpptReportingComputed().get(0)
														  .getTsComputed().get(0)
														  .getMonthComputed().get(0)
														  .getEpptReportingComputed().get(0)
														  .getMonthPeriod();
		String columnTitle = monthPeriod.getName();
		Set<Month> selectedMonths = selectedMonthlyPeriods.stream()
														  .map(period -> period.getYearMonths(1955))
														  .flatMap(Collection::stream)
														  .map(YearMonth::getMonth)
														  .collect(toSet());
		_aggregateColumnSpec = new RmaTreeTableColumnSpec.Builder(columnTitle)
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(selectedMonths.size() == 12)
				.build();
		Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap = processColumns(waterYearDefinition, _aggregateColumnSpec,
				selectedMonths);
		for(EpptReportingComputedSet.EpptReportingScenarioComputed computed : epptReportingComputedSet.getEpptReportingComputed())
		{
			for(EpptReportingComputedSet.EpptReportingTs tsComputed : computed.getTsComputed())
			{
				SummaryPaneRow.SummaryPaneRowScenario summaryPaneRowScenario = new SummaryPaneRow.SummaryPaneRowScenario(tsComputed.getTsName(),
						_rootColSpec);
				for(EpptReportingComputedSet.EpptReportingMonthComputed monthComputed : tsComputed.getMonthComputed())
				{
					List<EpptReportingComputedStatistics> computedStatistics = monthComputed.getEpptReportingComputed().get(0)
																							.getComputedStatistics();
					for(int statIndex = 0; statIndex < computedStatistics.size(); statIndex++)
					{
						SummaryPaneRow.SummaryPaneRowStat statRow = new SummaryPaneRow.SummaryPaneRowStat(summaryPaneRowScenario,
								computedStatistics.get(statIndex).getEpptStatistic(), _rootColSpec);
						for(EpptReportingComputed epptReportingComputed : monthComputed.getEpptReportingComputed())
						{
							processRows(statRow, monthTreeTableColumnSpecMap, epptReportingComputed, statIndex);
						}
						summaryPaneRowScenario.getChildren().add(statRow);
					}
				}
				getRows().add(summaryPaneRowScenario);
			}
		}
	}

	private void processRows(SummaryPaneRow.SummaryPaneRowStat statRow,
							 Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap,
							 EpptReportingComputed epptReportingComputed, int statIndex)
	{
		WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = epptReportingComputed.getWaterYearPeriodRangesFilter();
		List<EpptReportingComputedStatistics> statsList = epptReportingComputed.getComputedStatistics();
		if(statIndex < statsList.size())
		{
			EpptReportingComputedStatistics computedStatistics = statsList.get(statIndex);
			Map<TreeTableColumnSpec, Double> data = new HashMap<>();
			data.put(_aggregateColumnSpec, computedStatistics.getAggregateStatistic());
			for(Map.Entry<Month, Double> stat : computedStatistics.getStatisticallyComputedMonthly().entrySet())
			{
				data.put(monthTreeTableColumnSpecMap.get(stat.getKey()), stat.getValue());
			}

			String title = waterYearPeriodRangesFilter.getGroupName();
			if(!waterYearPeriodRangesFilter.getName().isEmpty())
			{
				title += " - " + waterYearPeriodRangesFilter.getName();
			}
			SummaryPaneRow.SummaryPaneRowData summaryPaneRowData = new SummaryPaneRow.SummaryPaneRowData(statRow, title, data, _rootColSpec);
			statRow.getChildren().add(summaryPaneRowData);
		}
	}

	private Map<Month, TreeTableColumnSpec> processColumns(WaterYearDefinition waterYearDefinition,
														   RmaTreeTableColumnSpec aggregateColumnSpec,
														   Set<Month> selectedMonths)
	{
		Map<Month, TreeTableColumnSpec> retval = new EnumMap<>(Month.class);
		getColumnSpecs().add(_rootColSpec);
		Month startMonth = waterYearDefinition.getStartMonth();
		do
		{
			RmaTreeTableColumnSpec spec = new RmaTreeTableColumnSpec.Builder(startMonth.getDisplayName(TextStyle.SHORT, Locale.getDefault()))
					.withCanBeHidden(true)
					.withEditable(false)
					.withSortable(false)
					.withVisibleByDefault(selectedMonths.contains(startMonth))
					.build();
			getColumnSpecs().add(spec);
			retval.put(startMonth, spec);
			startMonth = startMonth.plus(1);
		}
		while(startMonth != waterYearDefinition.getEndMonth().plus(1));
		getColumnSpecs().add(aggregateColumnSpec);
		return retval;
	}
}

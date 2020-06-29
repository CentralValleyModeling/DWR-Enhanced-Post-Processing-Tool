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

import java.time.LocalDateTime;
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
import java.util.SortedMap;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputed;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedStatistics;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
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
class MonthlySequentialTableModel extends MonthlyTableTableModel
{
	private final RmaTreeTableColumnSpec _waterYearColSpec;
	private final RmaTreeTableColumnSpec _aggregateColumnSpec;

	MonthlySequentialTableModel(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet epptReportingComputedSet,
								List<MonthPeriod> selectedMonthlyPeriods)
	{
		_waterYearColSpec = new RmaTreeTableColumnSpec.Builder(plotTitle + " (" + epptReportingComputedSet.getUnits() + ")")
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
				for(EpptReportingComputedSet.EpptReportingMonthComputed monthComputed : tsComputed.getMonthComputed())
				{
					for(EpptReportingComputed epptReportingComputed : monthComputed.getEpptReportingComputed())
					{
						processRows(monthTreeTableColumnSpecMap, monthPeriod, tsComputed.getTsName(),
								epptReportingComputed);
					}
				}
			}
		}
	}

	private void processRows(Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap, MonthPeriod monthPeriod,
							 String plotTitle, EpptReportingComputed primary)
	{
		SortedMap<LocalDateTime, Double> series = primary.getDiscreteSeries();
		if(!series.isEmpty())
		{
			SortedMap<Integer, Double> yearly = primary.getAggregateSeries();
			String title = plotTitle + " - " + primary.getWaterYearPeriodRangesFilter().getGroupName();
			if(!primary.getWaterYearPeriodRangesFilter().getName().isEmpty())
			{
				title += " - " + primary.getWaterYearPeriodRangesFilter().getName();
			}
			MonthlyPaneRow.MonthlyPaneRowScenario monthlyPaneRowScenario = new MonthlyPaneRow.MonthlyPaneRowScenario(0, title, _waterYearColSpec);
			getRows().add(monthlyPaneRowScenario);
			for(int year : yearly.keySet())
			{
				List<YearMonth> yearMonths = monthPeriod.getYearMonths(year);
				LocalDateTime startYearMonth = yearMonths.get(0).atEndOfMonth().minusDays(2).atTime(0, 0);
				LocalDateTime endYearMonth = yearMonths.get(yearMonths.size() - 1).atEndOfMonth().plusDays(2).atTime(0, 0);
				SortedMap<LocalDateTime, Double> yearValues = series.subMap(startYearMonth, endYearMonth);
				Map<TreeTableColumnSpec, Double> dataMap = new HashMap<>();
				for(Map.Entry<LocalDateTime, Double> entry : yearValues.entrySet())
				{
					LocalDateTime localDateTime = entry.getKey().minusMonths(1);
					dataMap.put(monthTreeTableColumnSpecMap.get(localDateTime.getMonth()), entry.getValue());
				}
				Double orDefault = yearly.getOrDefault(year, Double.NaN);
				if(!Double.isNaN(orDefault))
				{
					dataMap.put(_aggregateColumnSpec, orDefault);
					MonthlyPaneRow.MonthlyPaneRowData dataRow = new MonthlyPaneRow.MonthlyPaneRowData(Integer.toString(year), _waterYearColSpec);
					dataRow.addData(dataMap);
					monthlyPaneRowScenario.getChildren().add(dataRow);
				}
			}
			for(EpptReportingComputedStatistics computedStatistics : primary.getComputedStatistics())
			{
				EpptStatistic epptStatistic = computedStatistics.getEpptStatistic();
				Map<TreeTableColumnSpec, Double> dataMap = new HashMap<>();
				for(Map.Entry<Month, Double> entry : computedStatistics.getStatisticallyComputedMonthly().entrySet())
				{
					dataMap.put(monthTreeTableColumnSpecMap.get(entry.getKey()), entry.getValue());
				}
				dataMap.put(_aggregateColumnSpec, computedStatistics.getAggregateStatistic());
				MonthlyPaneRow.MonthlyPaneRowData monthlyPaneRowData = new MonthlyPaneRow.MonthlyPaneRowData(epptStatistic.getName(), _waterYearColSpec);
				monthlyPaneRowScenario.getChildren().add(monthlyPaneRowData);
				monthlyPaneRowData.addData(dataMap);
			}
		}
	}

	private Map<Month, TreeTableColumnSpec> processColumns(WaterYearDefinition waterYearDefinition,
														   RmaTreeTableColumnSpec aggregateColumnSpec,
														   Set<Month> selectedMonths)
	{
		Map<Month, TreeTableColumnSpec> retval = new EnumMap<>(Month.class);
		getColumnSpecs().add(_waterYearColSpec);
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

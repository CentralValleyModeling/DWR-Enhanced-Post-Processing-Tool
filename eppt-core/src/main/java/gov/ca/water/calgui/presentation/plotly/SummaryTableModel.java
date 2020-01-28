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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.function.Function;
import java.util.function.Supplier;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputed;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingScenarioComputed;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.project.EpptScenarioRun;

import com.rma.javafx.treetable.RmaTreeTableModel;
import com.rma.javafx.treetable.columns.specs.RmaTreeTableColumnSpec;
import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
class SummaryTableModel extends RmaTreeTableModel<SummaryPaneRow>
{
	static final RmaTreeTableColumnSpec ROOT_COL_SPEC;
	static final RmaTreeTableColumnSpec ALL_COL_SPEC;

	static
	{
		ROOT_COL_SPEC = new RmaTreeTableColumnSpec.Builder("")
				.withCanBeHidden(false)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
		ALL_COL_SPEC = new RmaTreeTableColumnSpec.Builder("")
				.withCanBeHidden(true)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
	}

	SummaryTableModel(String plotTitle, WaterYearDefinition waterYearDefinition,
					  SortedMap<String, SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>>> data)
	{
		Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap = processColumns(waterYearDefinition);
		for(Map.Entry<String, SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>>> rootEntry : data.entrySet())
		{
			processScenarioData(plotTitle, monthTreeTableColumnSpecMap, rootEntry, EpptReportingScenarioComputed::getPrimary);
			processScenarioData(plotTitle, monthTreeTableColumnSpecMap, rootEntry, EpptReportingScenarioComputed::getSecondary);
		}
	}

	private void processScenarioData(String plotTitle, Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap,
									 Map.Entry<String, SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>>> rootEntry,
									 Function<EpptReportingScenarioComputed, EpptReportingComputed> computedProvider)
	{
		SummaryPaneRow.SummaryPaneRowScenario secondaryRoot = new SummaryPaneRow.SummaryPaneRowScenario(rootEntry.getKey());
		for(Map.Entry<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>> statEntry : rootEntry.getValue().entrySet())
		{
			EpptStatistic key = statEntry.getKey();
			SummaryPaneRow.SummaryPaneRowStat statRow = new SummaryPaneRow.SummaryPaneRowStat(secondaryRoot, key);
			secondaryRoot.getChildren().add(statRow);
			if(!key.getName().isEmpty())
			{
				for(Map.Entry<WaterYearIndex, EpptReportingComputedSet> waterYearIndexEntry : statEntry.getValue().entrySet())
				{
					SummaryPaneRow.SummaryPaneRowWaterYearIndex indexRow = new SummaryPaneRow.SummaryPaneRowWaterYearIndex(statRow,
							waterYearIndexEntry.getKey());
					statRow.getChildren().add(indexRow);
					EpptReportingComputedSet epptReportingComputedSet = waterYearIndexEntry.getValue();
					List<EpptReportingScenarioComputed> epptReportingComputed = epptReportingComputedSet.getEpptReportingComputed();
					for(EpptReportingScenarioComputed computed : epptReportingComputed)
					{
						processRows(indexRow, monthTreeTableColumnSpecMap, plotTitle, computedProvider.apply(computed));
					}
				}
			}
		}
		getRows().add(secondaryRoot);
	}

	private void processRows(SummaryPaneRow.SummaryPaneRowWaterYearIndex parent, Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap,
							 String rowTitle, EpptReportingComputed primary)
	{
		List<SortedMap<Month, Double>> statisticallyComputedTimeSeriesWyt = primary.getStatisticallyComputedMonthly();
		List<Double> yearlyStatistic = primary.getYearlyStatistic();
		for(int i = 0; i < statisticallyComputedTimeSeriesWyt.size(); i++)
		{
			SortedMap<Month, Double> monthPeriodData = statisticallyComputedTimeSeriesWyt.get(i);
			Map<TreeTableColumnSpec, Double> data = new HashMap<>();
			for(Map.Entry<Month, Double> entry : monthPeriodData.entrySet())
			{
				data.put(monthTreeTableColumnSpecMap.get(entry.getKey()), entry.getValue());
			}
			data.put(ALL_COL_SPEC, yearlyStatistic.get(i));
			SummaryPaneRow.SummaryPaneRowData dataRow = new SummaryPaneRow.SummaryPaneRowData(parent, "All", data);
			parent.getChildren().add(dataRow);
		}
	}

	private Map<Month, TreeTableColumnSpec> processColumns(WaterYearDefinition waterYearDefinition)
	{
		Map<Month, TreeTableColumnSpec> retval = new HashMap<>();
		getColumnSpecs().add(ROOT_COL_SPEC);
		Month startMonth = waterYearDefinition.getStartMonth();
		do
		{
			RmaTreeTableColumnSpec spec = new RmaTreeTableColumnSpec.Builder(startMonth.getDisplayName(TextStyle.SHORT, Locale.getDefault()))
					.withCanBeHidden(true)
					.withEditable(false)
					.withSortable(false)
					.withVisibleByDefault(true)
					.build();
			getColumnSpecs().add(spec);
			retval.put(startMonth, spec);
			startMonth = startMonth.plus(1);
		}
		while(startMonth != waterYearDefinition.getEndMonth().plus(1));
		return retval;
	}
}

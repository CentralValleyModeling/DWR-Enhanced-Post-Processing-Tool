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
import java.time.format.TextStyle;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.function.Function;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputed;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingScenarioComputed;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;

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
	static final RmaTreeTableColumnSpec ANNUAL_COL_SPEC;

	static
	{
		ROOT_COL_SPEC = new RmaTreeTableColumnSpec.Builder("")
				.withCanBeHidden(false)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
		ANNUAL_COL_SPEC = new RmaTreeTableColumnSpec.Builder("Annual")
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
		}
	}

	private void processScenarioData(String plotTitle, Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap,
									 Map.Entry<String, SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>>> rootEntry,
									 Function<EpptReportingScenarioComputed, EpptReportingComputed> computedProvider)
	{
		SummaryPaneRow.SummaryPaneRowScenario secondaryRoot = new SummaryPaneRow.SummaryPaneRowScenario(rootEntry.getKey());
		getRows().add(secondaryRoot);
		int i = 0;
		for(Map.Entry<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>> statEntry : rootEntry.getValue().entrySet())
		{
			EpptStatistic key = statEntry.getKey();
			if(!key.getName().isEmpty())
			{
				SummaryPaneRow.SummaryPaneRowStat summaryPaneRowStat = new SummaryPaneRow.SummaryPaneRowStat(secondaryRoot, key);
				Double yearlyStat = null;
				SortedMap<Month, Double> monthPeriodData = new TreeMap<>();
				if(!statEntry.getValue().values().isEmpty())
				{
					EpptReportingComputed primary = statEntry.getValue().values().iterator().next().getEpptReportingComputed().get(0).getPrimary();
					List<SortedMap<Month, Double>> statisticallyComputedTimeSeriesWyt = primary.getStatisticallyComputedMonthly();
					List<Double> yearlyStatistics = primary.getYearlyStatistic();
					yearlyStat = yearlyStatistics.get(i);
					monthPeriodData = statisticallyComputedTimeSeriesWyt.get(i);
				}
				addAllRow(summaryPaneRowStat, monthTreeTableColumnSpecMap, yearlyStat, monthPeriodData);
				summaryPaneRowStat.getChildren().add(new SummaryPaneRow.SummaryPaneBlankRow(summaryPaneRowStat));
				for(Map.Entry<WaterYearIndex, EpptReportingComputedSet> waterYearIndexEntry : statEntry.getValue().entrySet())
				{
					WaterYearIndex waterYearIndex = waterYearIndexEntry.getKey();
					EpptReportingComputedSet epptReportingComputedSet = waterYearIndexEntry.getValue();
					List<EpptReportingScenarioComputed> epptReportingComputed = epptReportingComputedSet.getEpptReportingComputed();
					for(EpptReportingScenarioComputed computed : epptReportingComputed)
					{
						processRows(i, waterYearIndex, summaryPaneRowStat, monthTreeTableColumnSpecMap, computedProvider.apply(computed));
					}
				}
				secondaryRoot.getChildren().add(summaryPaneRowStat);
			}
		}
	}

	private void processRows(int i, WaterYearIndex waterYearIndex, SummaryPaneRow parent,
							 Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap,
							 EpptReportingComputed primary)
	{
		List<SortedMap<WaterYearPeriod, SortedMap<Month, Double>>> statisticallyComputedPeriodMonthly = primary.getStatisticallyComputedPeriodMonthly();
		List<SortedMap<WaterYearPeriod, Double>> annualWaterYearPeriod = primary.getStatisticallyComputedTimeSeriesWyt();
		SortedMap<WaterYearPeriod, Double> waterYearPeriodAnnualValues = annualWaterYearPeriod.get(i);
		SortedMap<WaterYearPeriod, SortedMap<Month, Double>> periodMonthMap = statisticallyComputedPeriodMonthly.get(i);
		addWaterYearPeriodRows(parent, waterYearIndex, monthTreeTableColumnSpecMap, waterYearPeriodAnnualValues, periodMonthMap);
	}

	private void addWaterYearPeriodRows(SummaryPaneRow parent,
										WaterYearIndex waterYearIndex,
										Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap,
										SortedMap<WaterYearPeriod, Double> waterYearPeriodAnnualValues,
										SortedMap<WaterYearPeriod, SortedMap<Month, Double>> periodMonthMap)
	{
		for(Map.Entry<WaterYearPeriod, SortedMap<Month, Double>> entry : periodMonthMap.entrySet())
		{
			WaterYearPeriod key = entry.getKey();
			SortedMap<Month, Double> monthData = entry.getValue();
			Map<TreeTableColumnSpec, Double> data = new HashMap<>();
			for(Map.Entry<Month, Double> monthDataEntry : monthData.entrySet())
			{
				data.put(monthTreeTableColumnSpecMap.get(monthDataEntry.getKey()), monthDataEntry.getValue());
			}
			data.put(ANNUAL_COL_SPEC, waterYearPeriodAnnualValues.get(key));
			SummaryPaneRow.SummaryPaneRowData dataRow = new SummaryPaneRow.SummaryPaneRowData(parent, waterYearIndex + " - " + key.getPeriodName(),
					data);
			parent.getChildren().add(dataRow);
		}
		parent.getChildren().add(new SummaryPaneRow.SummaryPaneBlankRow(parent));
	}

	private void addAllRow(SummaryPaneRow parent,
						   Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap,
						   Double yearlyStat, SortedMap<Month, Double> monthPeriodData)
	{
		Map<TreeTableColumnSpec, Double> data = new HashMap<>();
		for(Map.Entry<Month, Double> entry : monthPeriodData.entrySet())
		{
			data.put(monthTreeTableColumnSpecMap.get(entry.getKey()), entry.getValue());
		}
		data.put(ANNUAL_COL_SPEC, yearlyStat);
		SummaryPaneRow.SummaryPaneRowData dataRow = new SummaryPaneRow.SummaryPaneRowData(parent, "All", data);
		parent.getChildren().add(dataRow);
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
		getColumnSpecs().add(ANNUAL_COL_SPEC);
		return retval;
	}
}

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
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputed;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedStatistics;
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
class MonthlyTableModel extends RmaTreeTableModel<MonthlyPaneRow>
{
	static final RmaTreeTableColumnSpec WATER_YEAR_COL_SPEC;

	static
	{
		WATER_YEAR_COL_SPEC = new RmaTreeTableColumnSpec.Builder("WY")
				.withCanBeHidden(false)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
	}

	private final RmaTreeTableColumnSpec _aggregateColumnSpec;

	MonthlyTableModel(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet epptReportingComputedSet)
	{
		MonthPeriod monthPeriod = epptReportingComputedSet.getEpptReportingComputed().get(0)
														  .getTsComputed().get(0)
														  .getMonthComputed().get(0)
														  .getEpptReportingComputed().get(0)
														  .getMonthPeriod();
		String columnTitle = monthPeriod.getName();
		_aggregateColumnSpec = new RmaTreeTableColumnSpec.Builder(columnTitle)
				.withCanBeHidden(false)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
		Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap = processColumns(waterYearDefinition, _aggregateColumnSpec);
		for(EpptReportingComputedSet.EpptReportingScenarioComputed computed : epptReportingComputedSet.getEpptReportingComputed())
		{
			for(EpptReportingComputedSet.EpptReportingTs tsComputed : computed.getTsComputed())
			{
				for(EpptReportingComputedSet.EpptReportingMonthComputed monthComputed : tsComputed.getMonthComputed())
				{
					for(EpptReportingComputed epptReportingComputed : monthComputed.getEpptReportingComputed())
					{
						processRows(monthTreeTableColumnSpecMap, monthPeriod, tsComputed.getTsName(), epptReportingComputed);
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
			String title = plotTitle + " - " + primary.getWaterYearPeriodRangesFilter().getName();
			MonthlyPaneRow.MonthlyPaneRowScenario monthlyPaneRowScenario = new MonthlyPaneRow.MonthlyPaneRowScenario(title);
			getRows().add(monthlyPaneRowScenario);
			for(int year = series.firstKey().getYear(); year < series.lastKey().getYear(); year++)
			{
				List<YearMonth> yearMonths = monthPeriod.getYearMonths(year);
				LocalDateTime startYearMonth = yearMonths.get(0).atEndOfMonth().minusDays(2).atTime(0, 0);
				LocalDateTime endYearMonth = yearMonths.get(yearMonths.size() - 1).atEndOfMonth().plusDays(2).atTime(0, 0);
				if(series.firstKey().isAfter(startYearMonth) || series.lastKey().isBefore(endYearMonth))
				{
					continue;
				}
				SortedMap<LocalDateTime, Double> yearValues = series.subMap(startYearMonth, endYearMonth);
				Map<TreeTableColumnSpec, Double> dataMap = new HashMap<>();
				for(Map.Entry<LocalDateTime, Double> entry : yearValues.entrySet())
				{
					LocalDateTime localDateTime = entry.getKey().minusMonths(1);
					dataMap.put(monthTreeTableColumnSpecMap.get(localDateTime.getMonth()), entry.getValue());
				}
				dataMap.put(_aggregateColumnSpec, yearly.getOrDefault(year, Double.NaN));
				MonthlyPaneRow.MonthlyPaneRowData dataRow = new MonthlyPaneRow.MonthlyPaneRowData(monthlyPaneRowScenario, year, dataMap);
				monthlyPaneRowScenario.getChildren().add(dataRow);
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
				monthlyPaneRowScenario.getChildren().add(new MonthlyPaneRow.MonthlyPaneRowStat(monthlyPaneRowScenario, epptStatistic, dataMap));
			}
		}
	}

	private Map<Month, TreeTableColumnSpec> processColumns(WaterYearDefinition waterYearDefinition,
														   RmaTreeTableColumnSpec aggregateColumnSpec)
	{
		Map<Month, TreeTableColumnSpec> retval = new HashMap<>();
		getColumnSpecs().add(WATER_YEAR_COL_SPEC);
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
		getColumnSpecs().add(aggregateColumnSpec);
		return retval;
	}
}

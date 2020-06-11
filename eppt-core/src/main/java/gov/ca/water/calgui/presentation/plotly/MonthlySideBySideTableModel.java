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
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputed;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedStatistics;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.project.EpptScenarioRun;

import com.rma.javafx.treetable.columns.specs.RmaTreeTableColumnSpec;
import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
class MonthlySideBySideTableModel extends MonthlyTableTableModel
{
	private final RmaTreeTableColumnSpec _waterYearColSpec;
	private final RmaTreeTableColumnSpec _aggregateColumnSpec;
	private final WaterYearDefinition _waterYearDefinition;

	MonthlySideBySideTableModel(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet epptReportingComputedSet,
								List<MonthPeriod> selectedMonthlyPeriods)
	{
		_waterYearDefinition = waterYearDefinition;
		_waterYearColSpec = new RmaTreeTableColumnSpec.Builder(plotTitle + " (" + epptReportingComputedSet.getUnits() + ")").withCanBeHidden(false)
																															.withEditable(false)
																															.withSortable(false)
																															.withVisibleByDefault(true)
																															.build();
		List<EpptScenarioRun> scenarioRuns = epptReportingComputedSet.getEpptReportingComputed()
																	 .stream()
																	 .map(EpptReportingComputedSet.EpptReportingScenarioComputed::getEpptScenarioRun)
																	 .collect(toList());
		MonthPeriod monthPeriod = epptReportingComputedSet.getEpptReportingComputed()
														  .get(0)
														  .getTsComputed()
														  .get(0)
														  .getMonthComputed()
														  .get(0)
														  .getEpptReportingComputed()
														  .get(0)
														  .getMonthPeriod();
		String columnTitle = monthPeriod.getName();
		Set<Month> selectedMonths = selectedMonthlyPeriods.stream()
														  .map(period -> period.getYearMonths(1955))
														  .flatMap(Collection::stream)
														  .map(YearMonth::getMonth)
														  .collect(toSet());
		_aggregateColumnSpec = new RmaTreeTableColumnSpec.Builder(columnTitle).withCanBeHidden(true)
																			  .withEditable(false)
																			  .withSortable(false)
																			  .withVisibleByDefault(selectedMonths.size() == 12)
																			  .build();

		NavigableMap<MonthlyPaneRow.MonthlyPaneRowScenario, NavigableMap<Integer, MonthlyPaneRow.MonthlyPaneRowData>> rowMap = new TreeMap<>(
				Comparator.comparing(MonthlyPaneRow.MonthlyPaneRowScenario::getIndex));
		Map<MonthPeriod, SortedMap<EpptScenarioRun, TreeTableColumnSpec>> monthTreeTableColumnSpecMap = processColumns(waterYearDefinition, _aggregateColumnSpec,
				selectedMonths, scenarioRuns);
		for(EpptReportingComputedSet.EpptReportingScenarioComputed computed : epptReportingComputedSet.getEpptReportingComputed())
		{
			for(EpptReportingComputedSet.EpptReportingTs tsComputed : computed.getTsComputed())
			{
				for(EpptReportingComputedSet.EpptReportingMonthComputed monthComputed : tsComputed.getMonthComputed())
				{
					for(EpptReportingComputed epptReportingComputed : monthComputed.getEpptReportingComputed())
					{
						processRows(rowMap, monthTreeTableColumnSpecMap, computed.getEpptScenarioRun(), monthPeriod, epptReportingComputed, tsComputed.getSupplementalInfo());
					}
				}
			}
		}
		for(Map.Entry<MonthlyPaneRow.MonthlyPaneRowScenario, NavigableMap<Integer, MonthlyPaneRow.MonthlyPaneRowData>> entry : rowMap.entrySet())
		{
			MonthlyPaneRow.MonthlyPaneRowScenario key = entry.getKey();
			getRows().add(key);
			entry.getValue().values().forEach(r -> key.getChildren().add(r));
		}
	}

	private void processRows(NavigableMap<MonthlyPaneRow.MonthlyPaneRowScenario, NavigableMap<Integer, MonthlyPaneRow.MonthlyPaneRowData>> rowMap,
							 Map<MonthPeriod, SortedMap<EpptScenarioRun, TreeTableColumnSpec>> monthTreeTableColumnSpecMap, EpptScenarioRun epptScenarioRun,
							 MonthPeriod monthPeriod, EpptReportingComputed primary, String supplementalInfo)
	{
		SortedMap<LocalDateTime, Double> series = primary.getDiscreteSeries();
		if(!series.isEmpty())
		{
			SortedMap<Integer, Double> yearly = primary.getAggregateSeries();
			String title = "";
			if(supplementalInfo != null)
			{
				title += supplementalInfo + " - ";
			}
			title += primary.getWaterYearPeriodRangesFilter().getGroupName();
			if(!primary.getWaterYearPeriodRangesFilter().getName().isEmpty())
			{
				title += " - " + primary.getWaterYearPeriodRangesFilter().getName();
			}
			int index = 0;
			if(!rowMap.isEmpty())
			{
				String rowTitle = title;
				index = rowMap.keySet()
							  .stream()
							  .filter(s -> s.getTitle().equals(rowTitle))
							  .map(MonthlyPaneRow.MonthlyPaneRowScenario::getIndex)
							  .findAny()
							  .orElse(rowMap.lastKey().getIndex() + 1);
			}
			NavigableMap<Integer, MonthlyPaneRow.MonthlyPaneRowData> integerMonthlyPaneRowDataNavigableMap = rowMap.computeIfAbsent(
					new MonthlyPaneRow.MonthlyPaneRowScenario(index, title, _waterYearColSpec), e -> new TreeMap<>());
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
					dataMap.put(monthTreeTableColumnSpecMap.get(new MonthPeriod("", localDateTime.getMonth(), localDateTime.getMonth())).get(epptScenarioRun),
							entry.getValue());
				}
				Double orDefault = yearly.getOrDefault(year, Double.NaN);
				if(!Double.isNaN(orDefault))
				{
					dataMap.put(monthTreeTableColumnSpecMap.get(new MonthPeriod("", _waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth()))
														   .get(epptScenarioRun), orDefault);
				}
				MonthlyPaneRow.MonthlyPaneRowData monthlyPaneRowData = integerMonthlyPaneRowDataNavigableMap.computeIfAbsent(year,
						y -> new MonthlyPaneRow.MonthlyPaneRowData(Integer.toString(y), _waterYearColSpec));
				monthlyPaneRowData.addData(dataMap);
			}
			int fakeYear = yearly.lastKey() + 1;
			for(EpptReportingComputedStatistics computedStatistics : primary.getComputedStatistics())
			{
				EpptStatistic epptStatistic = computedStatistics.getEpptStatistic();
				Map<TreeTableColumnSpec, Double> dataMap = new HashMap<>();
				for(Map.Entry<Month, Double> entry : computedStatistics.getStatisticallyComputedMonthly().entrySet())
				{
					dataMap.put(monthTreeTableColumnSpecMap.get(new MonthPeriod("", entry.getKey(), entry.getKey())).get(epptScenarioRun), entry.getValue());
				}
				dataMap.put(
						monthTreeTableColumnSpecMap.get(new MonthPeriod("", _waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth())).get(epptScenarioRun),
						computedStatistics.getAggregateStatistic());
				MonthlyPaneRow.MonthlyPaneRowData monthlyPaneRowData = integerMonthlyPaneRowDataNavigableMap.computeIfAbsent(fakeYear,
						y -> new MonthlyPaneRow.MonthlyPaneRowData(epptStatistic.getName(), _waterYearColSpec));

				monthlyPaneRowData.addData(dataMap);
				fakeYear++;
			}
		}
	}

	private Map<MonthPeriod, SortedMap<EpptScenarioRun, TreeTableColumnSpec>> processColumns(WaterYearDefinition waterYearDefinition,
																							 RmaTreeTableColumnSpec aggregateColumnSpec, Set<Month> selectedMonths,
																							 List<EpptScenarioRun> epptScenarioRuns)
	{
		Map<MonthPeriod, SortedMap<EpptScenarioRun, TreeTableColumnSpec>> retval = new HashMap<>();
		getColumnSpecs().add(_waterYearColSpec);
		Month startMonth = waterYearDefinition.getStartMonth();
		do
		{
			String displayName = startMonth.getDisplayName(TextStyle.SHORT, Locale.getDefault());
			RmaTreeTableColumnSpec spec = new RmaTreeTableColumnSpec.Builder(displayName).withCanBeHidden(true)
																						 .withEditable(false)
																						 .withSortable(false)
																						 .withVisibleByDefault(selectedMonths.contains(startMonth))
																						 .build();
			getColumnSpecs().add(spec);
			for(EpptScenarioRun epptScenarioRun : epptScenarioRuns)
			{
				SortedMap<EpptScenarioRun, TreeTableColumnSpec> scenarioColumns = retval.computeIfAbsent(new MonthPeriod("", startMonth, startMonth),
						m -> new TreeMap<>(Comparator.comparing(epptScenarioRuns::indexOf)));
				RmaTreeTableColumnSpec scenarioSpec = new RmaTreeTableColumnSpec.Builder(epptScenarioRun.getName())
				{
					@Override
					public RmaTreeTableColumnSpec build()
					{
						return new RmaTreeTableColumnSpec(this.getColumnName(), this.isEditable(), this.isSortable(), this.isVisibleByDefault(), this.canBeHidden(),
								this.getChildren())
						{
							@Override
							public String toString()
							{
								return spec.getColumnName() + " - " + getColumnName();
							}
						};
					}
				}.withCanBeHidden(true).withEditable(false).withSortable(false).withVisibleByDefault(selectedMonths.contains(startMonth)).build();
				spec.getChildren().add(scenarioSpec);
				scenarioColumns.put(epptScenarioRun, scenarioSpec);
			}
			startMonth = startMonth.plus(1);
		} while(startMonth != waterYearDefinition.getEndMonth().plus(1));
		getColumnSpecs().add(aggregateColumnSpec);
		for(EpptScenarioRun epptScenarioRun : epptScenarioRuns)
		{
			SortedMap<EpptScenarioRun, TreeTableColumnSpec> scenarioColumns = retval.computeIfAbsent(
					new MonthPeriod("", _waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth()),
					m -> new TreeMap<>(Comparator.comparing(epptScenarioRuns::indexOf)));
			RmaTreeTableColumnSpec scenarioSpec = new RmaTreeTableColumnSpec.Builder(epptScenarioRun.getName()).withCanBeHidden(true)
																											   .withEditable(false)
																											   .withSortable(false)
																											   .withVisibleByDefault(true)
																											   .build();
			aggregateColumnSpec.getChildren().add(scenarioSpec);
			scenarioColumns.put(epptScenarioRun, scenarioSpec);
		}
		return retval;
	}
}

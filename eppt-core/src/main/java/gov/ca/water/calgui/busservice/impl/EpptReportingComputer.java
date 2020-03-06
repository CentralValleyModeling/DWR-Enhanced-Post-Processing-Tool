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

package gov.ca.water.calgui.busservice.impl;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.YearMonth;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.SortedMap;
import java.util.TreeMap;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssCache;
import gov.ca.water.calgui.scripts.DssReader;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import rma.util.RMAConst;

import static java.util.stream.Collectors.toMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-26-2019
 */
public final class EpptReportingComputer
{
	private final List<EpptStatistic> _statistics;
	private final MonthPeriod _monthPeriod;
	private final Map<EpptScenarioRun, WaterYearPeriodRangesFilter> _waterYearPeriodRangesFilters;
	private final WaterYearDefinition _waterYearDefinition;

	private EpptReportingComputer(List<EpptStatistic> statistics, WaterYearDefinition waterYearDefinition, MonthPeriod monthPeriod,
								  Map<EpptScenarioRun, WaterYearPeriodRangesFilter> waterYearPeriodRangesFilters)
	{
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_waterYearPeriodRangesFilters = waterYearPeriodRangesFilters;
		_waterYearDefinition = waterYearDefinition;
	}

	public static EpptReportingComputedSet computeForMetrics(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
															 String plotTitle, boolean taf, WaterYearDefinition waterYearDefinition,
															 List<MonthPeriod> monthPeriods,
															 List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> waterYearPeriodRangesFilters,
															 List<EpptStatistic> statistic)
	{
		List<EpptReportingComputedSet.EpptReportingScenarioComputed> trendReportingComputed = new ArrayList<>();

		for(Map.Entry<EpptScenarioRun, List<TimeSeriesContainer>> data : scenarioRunData.entrySet())
		{
			List<EpptReportingComputedSet.EpptReportingTs> tsComputes = new ArrayList<>();
			for(TimeSeriesContainer ts : data.getValue())
			{
				List<EpptReportingComputedSet.EpptReportingMonthComputed> monthComputes = new ArrayList<>();
				for(MonthPeriod monthPeriod : monthPeriods)
				{
					List<EpptReportingComputed> annualComputes = new ArrayList<>();
					for(Map<EpptScenarioRun, WaterYearPeriodRangesFilter> waterYearPeriodRangesFilter : waterYearPeriodRangesFilters)
					{
						EpptReportingComputer trendReportingComputer = new EpptReportingComputer(statistic, waterYearDefinition, monthPeriod,
								waterYearPeriodRangesFilter);
						EpptReportingComputed compute = trendReportingComputer.compute(data.getKey(), ts, taf);
						annualComputes.add(compute);
					}
					monthComputes.add(new EpptReportingComputedSet.EpptReportingMonthComputed(annualComputes));
				}
				String name = "";
				if(ts != null)
				{
					name = ts.getFullName();
				}
				tsComputes.add(new EpptReportingComputedSet.EpptReportingTs(name, monthComputes));
			}
			trendReportingComputed.add(new EpptReportingComputedSet.EpptReportingScenarioComputed(data.getKey(), tsComputes));
		}
		return new EpptReportingComputedSet(plotTitle, trendReportingComputed);
	}

	private EpptReportingComputed compute(EpptScenarioRun scenarioRun, TimeSeriesContainer primarySeries, boolean convertTaf)
	{
		DssReader dssReader = new DssReader(scenarioRun, _waterYearDefinition, new DssCache());
		TimeSeriesContainer timeSeriesContainer = new TimeSeriesContainer();
		if(primarySeries != null)
		{
			primarySeries.clone(timeSeriesContainer);
		}
		dssReader.setOriginalUnits(timeSeriesContainer.getUnits());
		boolean aggregateYearly = isAggregateYearly(convertTaf, timeSeriesContainer);
		TimeSeriesContainer[] data = {timeSeriesContainer};
		WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = _waterYearPeriodRangesFilters.get(scenarioRun);
		NavigableMap<LocalDateTime, Double> discreteSeries = dssReader.timeSeriesContainerToMap(data, convertTaf)
																	  .entrySet()
																	  .stream()
																	  .filter(waterYearPeriodRangesFilter)
																	  .collect(toMap(Map.Entry::getKey, Map.Entry::getValue,
																			  (o1, o2) -> o1, TreeMap::new));
		NavigableMap<Integer, Double> aggregateSeries = DssReader.filterPeriodYearly(discreteSeries, _monthPeriod, aggregateYearly);
		List<EpptReportingComputedStatistics> computedStatistics = computeStatistics(discreteSeries, aggregateSeries);
		return new EpptReportingComputed(_monthPeriod, waterYearPeriodRangesFilter, discreteSeries, aggregateSeries, computedStatistics);
	}

	private List<EpptReportingComputedStatistics> computeStatistics(NavigableMap<LocalDateTime, Double> discreteSeries,
																	NavigableMap<Integer, Double> aggregateSeries)
	{
		List<EpptReportingComputedStatistics> retval = new ArrayList<>();
		if(!discreteSeries.isEmpty())
		{
			for(EpptStatistic epptStatistic : _statistics)
			{
				SortedMap<Month, Double> monthlySplit = filterPeriodMonthly(discreteSeries, epptStatistic);
				Double aggregateStatistic = epptStatistic.calculateYearly(new ArrayList<>(aggregateSeries.values()));
				retval.add(new EpptReportingComputedStatistics(epptStatistic, aggregateStatistic, monthlySplit));
			}
		}
		return retval;
	}

	private String getUnits(TimeSeriesContainer tsc)
	{
		String units = "";
		if(tsc != null)
		{
			units = tsc.getParameterName() + " (" + tsc.getUnits() + ")";
		}
		return units;
	}

	private boolean isAggregateYearly(boolean convertTaf, TimeSeriesContainer primarySeries)
	{
		boolean aggregateYearly = false;
		if(primarySeries != null)
		{
			aggregateYearly = Constant.isAggregateYearly(convertTaf, primarySeries.getParameterName(), primarySeries.getUnits());
		}
		return aggregateYearly;
	}

	private SortedMap<Month, Double> filterPeriodMonthly(NavigableMap<LocalDateTime, Double> input, EpptStatistic epptStatistic)
	{
		SortedMap<Month, NavigableMap<Integer, Double>> monthlyMap = new TreeMap<>();
		if(!input.isEmpty())
		{
			for(int year = input.firstKey().getYear(); year < input.lastKey().getYear(); year++)
			{
				List<YearMonth> yearMonths = _monthPeriod.getYearMonths(year);
				LocalDateTime startYearMonth = yearMonths.get(0).atEndOfMonth().minusDays(2).atTime(0, 0);
				LocalDateTime endYearMonth = yearMonths.get(yearMonths.size() - 1).atEndOfMonth().plusDays(2).atTime(0, 0);
				if(input.firstKey().isAfter(startYearMonth) || input.lastKey().isBefore(endYearMonth))
				{
					continue;
				}
				SortedMap<LocalDateTime, Double> yearValues = input.subMap(startYearMonth, true, endYearMonth, true);
				for(Map.Entry<LocalDateTime, Double> entry : yearValues.entrySet())
				{
					LocalDateTime localDateTime = entry.getKey().minusMonths(1);
					NavigableMap<Integer, Double> yearlyMap = monthlyMap.computeIfAbsent(localDateTime.getMonth(), v -> new TreeMap<>());
					yearlyMap.put(localDateTime.getYear(), entry.getValue());
				}
			}
		}
		return monthlyMap.entrySet().stream()
						 .collect(toMap(Map.Entry::getKey, e -> epptStatistic.calculateYearly(new ArrayList<>(e.getValue().values())),
								 (o1, o2) -> o1, TreeMap::new));
	}
}

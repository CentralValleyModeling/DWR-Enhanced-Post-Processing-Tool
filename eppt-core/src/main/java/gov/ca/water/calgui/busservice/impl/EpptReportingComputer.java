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
public class EpptReportingComputer
{
	private final List<EpptStatistic> _statistics;
	private final MonthPeriod _monthPeriod;
	private final WaterYearPeriodRangesFilter _waterYearPeriodRangesFilter;
	private final WaterYearDefinition _waterYearDefinition;

	EpptReportingComputer(List<EpptStatistic> statistics, WaterYearDefinition waterYearDefinition, MonthPeriod monthPeriod,
						  WaterYearPeriodRangesFilter waterYearPeriodRangesFilter)
	{
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_waterYearPeriodRangesFilter = waterYearPeriodRangesFilter;
		_waterYearDefinition = waterYearDefinition;
	}

	public static EpptReportingComputedSet computeForMetrics(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
															 String plotTitle, boolean taf, WaterYearDefinition waterYearDefinition,
															 List<MonthPeriod> monthPeriods,
															 List<WaterYearPeriodRangesFilter> waterYearPeriodRangesFilters,
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
					for(WaterYearPeriodRangesFilter waterYearPeriodRangesFilter : waterYearPeriodRangesFilters)
					{
						EpptReportingComputer trendReportingComputer = new EpptReportingComputer(statistic, waterYearDefinition, monthPeriod,
								waterYearPeriodRangesFilter);
						EpptReportingComputed compute = trendReportingComputer.compute(data.getKey(), ts, taf);
						annualComputes.add(compute);
					}
					monthComputes.add(new EpptReportingComputedSet.EpptReportingMonthComputed(annualComputes));
				}
				tsComputes.add(new EpptReportingComputedSet.EpptReportingTs(ts.fullName, monthComputes));
			}
			trendReportingComputed.add(new EpptReportingComputedSet.EpptReportingScenarioComputed(data.getKey(), tsComputes));
		}
		return new EpptReportingComputedSet(plotTitle, trendReportingComputed);
	}

	private EpptReportingComputed compute(EpptScenarioRun scenarioRun, TimeSeriesContainer primarySeries, boolean convertTaf)
	{
		DssReader dssReader = new DssReader(scenarioRun, _waterYearDefinition, new DssCache());
		TimeSeriesContainer timeSeriesContainer = new TimeSeriesContainer();
		primarySeries.clone(timeSeriesContainer);
		dssReader.setOriginalUnits(timeSeriesContainer.getUnits());
		boolean aggregateYearly = isAggregateYearly(convertTaf, timeSeriesContainer);
		NavigableMap<LocalDateTime, Double> discreteSeries = dssReader.timeSeriesContainerToMap(new TimeSeriesContainer[]{timeSeriesContainer},
				convertTaf);
		NavigableMap<Integer, Double> aggregateSeries = DssReader.filterPeriodYearly(discreteSeries, _monthPeriod, aggregateYearly);
		List<EpptReportingComputedStatistics> computedStatistics = computeStatistics(discreteSeries, aggregateSeries);
		return new EpptReportingComputed(_monthPeriod, _waterYearPeriodRangesFilter, discreteSeries, aggregateSeries, computedStatistics);
	}

	private List<EpptReportingComputedStatistics> computeStatistics(NavigableMap<LocalDateTime, Double> discreteSeries,
																	NavigableMap<Integer, Double> aggregateSeries)
	{
		List<EpptReportingComputedStatistics> retval = new ArrayList<>();
		for(EpptStatistic epptStatistic : _statistics)
		{
			SortedMap<Month, Double> monthlySplit = filterPeriodMonthly(discreteSeries, epptStatistic);
			Double aggregateStatistic = epptStatistic.calculateYearly(new ArrayList<>(aggregateSeries.values()));
			retval.add(new EpptReportingComputedStatistics(epptStatistic, aggregateStatistic, monthlySplit));
		}
		return retval;
	}

	NavigableMap<LocalDateTime, Double> getFullTimeSeries(LocalDate start, LocalDate end, int offset,
														  TimeSeriesContainer tsc)
	{
		NavigableMap<LocalDateTime, Double> fullSeries = new TreeMap<>();
		if(tsc != null)
		{
			for(int i = 0; i < tsc.getNumberValues(); i++)
			{
				HecTime hecTime = tsc.getHecTime(i);
				double value = tsc.getValue(i);
				if(RMAConst.isValidValue(value) && value != -3.402823466E38)
				{
					Date javaDate = hecTime.getJavaDate(offset);
					fullSeries.put(LocalDateTime.ofInstant(javaDate.toInstant(), ZoneId.systemDefault()), value);
				}
			}
			if(!fullSeries.isEmpty())
			{
				fullSeries = fullSeries.subMap(start.minusDays(1).atTime(0, 0), true,
						end.plusDays(2).atTime(0, 0), true);
			}
		}
		return fullSeries;
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

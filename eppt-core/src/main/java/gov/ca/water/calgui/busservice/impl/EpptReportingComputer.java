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
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.WaterYearAnnualPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssReader;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import rma.util.RMAConst;

import static gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl.CFS_2_TAF_DAY;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-26-2019
 */
class EpptReportingComputer
{
	private static final Logger LOGGER = Logger.getLogger(EpptReportingComputer.class.getName());
	private final EpptStatistic _statistics;
	private final MonthPeriod _monthPeriod;
	private final Map<EpptScenarioRun, WaterYearIndex> _selectedIndexes;
	private final Map<EpptScenarioRun, List<WaterYearIndex>> _allIndexes;
	private final WaterYearDefinition _waterYearDefinition;

	EpptReportingComputer(EpptStatistic statistics, MonthPeriod monthPeriod,
						  Map<EpptScenarioRun, WaterYearIndex> selectedIndexes, Map<EpptScenarioRun, List<WaterYearIndex>> allIndexes)
	{
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_selectedIndexes = selectedIndexes;
		_allIndexes = allIndexes;
		_waterYearDefinition = new WaterYearDefinition("Default", Month.OCTOBER, Month.SEPTEMBER);
	}

	EpptReportingScenarioComputed computeCfs(EpptScenarioRun scenarioRun, List<String> primarySuffixes, List<TimeSeriesContainer> primarySeries,
											 List<String> secondarySuffixes, List<TimeSeriesContainer> secondarySeries,
											 LocalDate start, LocalDate end)
	{
		return compute(scenarioRun, primarySuffixes, primarySeries, secondarySuffixes, secondarySeries, false, start, end);
	}

	EpptReportingScenarioComputed computeTaf(EpptScenarioRun scenarioRun, List<String> primarySuffixes, List<TimeSeriesContainer> primarySeries,
											 List<String> secondarySuffixes, List<TimeSeriesContainer> secondarySeries,
											 LocalDate start, LocalDate end)
	{
		return compute(scenarioRun, primarySuffixes, primarySeries, secondarySuffixes, secondarySeries, true, start, end);
	}

	private EpptReportingScenarioComputed compute(EpptScenarioRun scenarioRun, List<String> primaryDataSuffixes,
												  List<TimeSeriesContainer> primarySeries, List<String> secondaryDataSuffixes,
												  List<TimeSeriesContainer> secondarySeries,
												  boolean convertTaf, LocalDate start, LocalDate end)
	{
		return new EpptReportingScenarioComputed(scenarioRun, compute(scenarioRun, primaryDataSuffixes, primarySeries, convertTaf, start, end),
				compute(scenarioRun, secondaryDataSuffixes, secondarySeries, convertTaf, start, end));
	}

	private EpptReportingComputed compute(EpptScenarioRun scenarioRun, List<String> dataSuffixes, List<TimeSeriesContainer> primarySeries,
										  boolean convertTaf,
										  LocalDate start, LocalDate end)
	{
		int offset = (int) TimeUnit.MILLISECONDS.toMinutes(TimeZone.getDefault().getRawOffset());
		List<SortedMap<LocalDateTime, Double>> fullSeries = new ArrayList<>();
		List<SortedMap<Integer, Double>> filteredPeriodYearly = new ArrayList<>();
		List<String> units = new ArrayList<>();
		List<SortedMap<WaterYearPeriod, Double>> waterYearPeriodGroupedYearly = new ArrayList<>();
		List<SortedMap<Month, Double>> monthly = new ArrayList<>();
		List<Double> yearlyStatistic = new ArrayList<>();
		for(int i = 0; i < primarySeries.size(); i++)
		{
			TimeSeriesContainer tsc = primarySeries.get(i);
			boolean aggregateYearly = isAggregateYearly(convertTaf, tsc);
			if(convertTaf)
			{
				calcTAFforCFS(tsc);
			}
			units.add(getUnits(tsc));
			NavigableMap<LocalDateTime, Double> full = getFullTimeSeries(start, end, offset, tsc, i);
			fullSeries.add(full);
			NavigableMap<Integer, Double> yearly = DssReader.filterPeriodYearly(full, _monthPeriod, aggregateYearly);
			filteredPeriodYearly.add(yearly);
			SortedMap<Month, NavigableMap<Integer, Double>> monthlySplit = filterPeriodMonthly(full);
			waterYearPeriodGroupedYearly.add(groupWaterYearPeriod(scenarioRun, yearly));
			monthly.add(_statistics.calculateMonthly(monthlySplit, _waterYearDefinition,
					getWaterYearIndexForScenario(scenarioRun), getWaterYearIndicesForScenario(scenarioRun), _monthPeriod));
			yearlyStatistic.add(_statistics.calculateYearly(yearly, _waterYearDefinition,
					getWaterYearIndexForScenario(scenarioRun),
					getWaterYearIndicesForScenario(scenarioRun)));
		}
		return new EpptReportingComputed(dataSuffixes, fullSeries, filteredPeriodYearly, yearlyStatistic, monthly,
				waterYearPeriodGroupedYearly, units);
	}

	NavigableMap<LocalDateTime, Double> getFullTimeSeries(LocalDate start, LocalDate end, int offset,
														  TimeSeriesContainer tsc, int index)
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

	private void calcTAFforCFS(TimeSeriesContainer tsc)
	{
		if(tsc != null)
		{
			try
			{
				if("CFS".equalsIgnoreCase(tsc.getUnits()))
				{
					HecTime ht = new HecTime();
					Calendar calendar = Calendar.getInstance();
					// Primary series
					for(int j = 0; j < tsc.numberValues; j++)
					{

						ht.set(tsc.times[j]);
						calendar.set(ht.year(), ht.month() - 1, 1);
						double monthlyTAF = tsc.values[j]
								* calendar.getActualMaximum(Calendar.DAY_OF_MONTH) * CFS_2_TAF_DAY;
						tsc.values[j] = monthlyTAF;
					}
				}
			}
			catch(RuntimeException ex)
			{
				LOGGER.log(Level.SEVERE, "Unable to calculate TAF.", ex);
			}
		}
	}

	private SortedMap<WaterYearPeriod, Double> groupWaterYearPeriod(EpptScenarioRun epptScenarioRun,
																	NavigableMap<Integer, Double> filteredYearlyPeriod)
	{
		WaterYearIndex waterYearIndexForScenario = getWaterYearIndexForScenario(epptScenarioRun);
		List<WaterYearPeriod> sortedPeriods = waterYearIndexForScenario.getSortedPeriods();
		if(sortedPeriods.stream().anyMatch(WaterYearPeriod::isDry) && sortedPeriods.stream().anyMatch(WaterYearPeriod::isCritical))
		{
			sortedPeriods.add(new WaterYearPeriod("Dry & Critical"));
		}
		SortedMap<WaterYearPeriod, Double> retval = new TreeMap<>(Comparator.comparingInt(sortedPeriods::indexOf));
		List<WaterYearIndex> waterYearIndicesForScenario = getWaterYearIndicesForScenario(epptScenarioRun);
		for(WaterYearPeriod waterYearPeriod : sortedPeriods)
		{
			List<WaterYearPeriodRange> periodRange = waterYearIndexForScenario.getWaterYearTypeGroups().getOrDefault(waterYearPeriod,
					new ArrayList<>())
																			  .stream()
																			  .map(e -> new WaterYearPeriodRange(e.getWaterYearPeriod(), e, e))
																			  .collect(toList());
			WaterYearAnnualPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearAnnualPeriodRangesFilter(periodRange);
			NavigableMap<Integer, Double> grouped = filteredYearlyPeriod.entrySet()
																		.stream()
																		.filter(waterYearPeriodRangesFilter)
																		.collect(toMap(Map.Entry::getKey, Map.Entry::getValue, (o, n) -> n,
																				TreeMap::new));
			Double calculate = _statistics.calculateYearly(grouped, _waterYearDefinition, waterYearIndexForScenario, waterYearIndicesForScenario);
			retval.put(waterYearPeriod, calculate);
		}
		return retval;
	}

	private SortedMap<Month, NavigableMap<Integer, Double>> filterPeriodMonthly(NavigableMap<LocalDateTime, Double> input)
	{
		SortedMap<Month, NavigableMap<Integer, Double>> retval = new TreeMap<>();
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
					NavigableMap<Integer, Double> yearlyMap = retval.computeIfAbsent(localDateTime.getMonth(), v -> new TreeMap<>());
					yearlyMap.put(localDateTime.getYear(), entry.getValue());
				}
			}
		}

		return retval;
	}

	private WaterYearIndex getWaterYearIndexForScenario(EpptScenarioRun epptScenarioRun)
	{
		return _selectedIndexes.get(epptScenarioRun);
	}

	private List<WaterYearIndex> getWaterYearIndicesForScenario(EpptScenarioRun epptScenarioRun)
	{
		return _allIndexes.get(epptScenarioRun);
	}
}

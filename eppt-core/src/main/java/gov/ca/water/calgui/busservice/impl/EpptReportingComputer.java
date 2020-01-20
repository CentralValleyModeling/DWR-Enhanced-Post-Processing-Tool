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
import java.util.Collections;
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

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearAnnualPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssReader;
import vista.set.TimeSeries;

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
public class EpptReportingComputer
{
	private static final Logger LOGGER = Logger.getLogger(EpptReportingComputer.class.getName());
	private final GUILinksAllModelsBO _guiLink;
	private final EpptStatistic _statistics;
	private final MonthPeriod _monthPeriod;
	private final Map<EpptScenarioRun, WaterYearIndex> _selectedIndicies;
	private final Map<EpptScenarioRun, List<WaterYearIndex>> _allIndicies;
	private final WaterYearDefinition _waterYearDefinition;

	public EpptReportingComputer(GUILinksAllModelsBO guiLink, EpptStatistic statistics, MonthPeriod monthPeriod,
								 Map<EpptScenarioRun, WaterYearIndex> selectedIndicies, Map<EpptScenarioRun, List<WaterYearIndex>> allIndicies)
	{
		_guiLink = guiLink;
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_selectedIndicies = selectedIndicies;
		_allIndicies = allIndicies;
		_waterYearDefinition = new WaterYearDefinition("Default", Month.OCTOBER, Month.SEPTEMBER);
	}

	public EpptReportingComputed computeCfs(EpptScenarioRun scenarioRun, TimeSeriesContainer[] primarySeries,
											LocalDate start, LocalDate end) throws EpptInitializationException
	{
		return compute(scenarioRun, primarySeries, false, start, end);
	}

	public EpptReportingComputed computeTaf(EpptScenarioRun scenarioRun, TimeSeriesContainer[] primarySeries,
											LocalDate start, LocalDate end) throws EpptInitializationException
	{
		return compute(scenarioRun, primarySeries, true, start, end);
	}

	private EpptReportingComputed compute(EpptScenarioRun scenarioRun, TimeSeriesContainer[] primarySeries,
										  boolean convertTaf, LocalDate start, LocalDate end) throws EpptInitializationException
	{
		int offset = (int) TimeUnit.MILLISECONDS.toMinutes(TimeZone.getDefault().getRawOffset());
		String units = getUnits(primarySeries);
		boolean aggregateYearly = isAggregateYearly(convertTaf, primarySeries);
		NavigableMap<LocalDateTime, Double> fullSeries = getFullTimeSeries(start, end, offset, primarySeries);
		NavigableMap<Integer, Double> filteredPeriodYearly = DssReader.filterPeriodYearly(fullSeries, _monthPeriod, aggregateYearly);
		SortedMap<Month, NavigableMap<Integer, Double>> filteredPeriodMonthly = filterPeriodMonthly(fullSeries);
		SortedMap<WaterYearPeriod, Double> waterYearPeriodGroupedYearly = groupWaterYearPeriod(scenarioRun, filteredPeriodYearly);
		SortedMap<Month, Double> monthly = _statistics.calculateMonthly(filteredPeriodMonthly, _waterYearDefinition,
				getWaterYearIndexForScenario(scenarioRun),
				getWaterYearIndicesForScenario(scenarioRun),
				_monthPeriod);
		Double yearlyStatistic = _statistics.calculateYearly(filteredPeriodYearly, _waterYearDefinition, getWaterYearIndexForScenario(scenarioRun),
				getWaterYearIndicesForScenario(scenarioRun));
		return new EpptReportingComputed(scenarioRun, fullSeries, filteredPeriodYearly, yearlyStatistic, monthly, waterYearPeriodGroupedYearly,
				units);
	}

	NavigableMap<LocalDateTime, Double> getFullTimeSeries(LocalDate start, LocalDate end, int offset,
														  TimeSeriesContainer[] primarySeries)
	{
		NavigableMap<LocalDateTime, Double> fullSeries = new TreeMap<>();
		if(primarySeries != null && primarySeries.length > 0 && primarySeries[0] != null)
		{
			TimeSeriesContainer tsc = primarySeries[0];
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

	private String getUnits(TimeSeriesContainer[] primarySeries)
	{
		String units = "";
		if(primarySeries != null && primarySeries.length > 0 && primarySeries[0] != null)
		{
			TimeSeriesContainer tsc = primarySeries[0];
			units = tsc.getParameterName() + " (" + tsc.getUnits() + ")";
		}
		return units;
	}

	private boolean isAggregateYearly(boolean convertTaf, TimeSeriesContainer[] primarySeries)
	{
		boolean aggregateYearly = false;
		if(primarySeries != null && primarySeries.length > 0 && primarySeries[0] != null)
		{
			if(convertTaf)
			{
				aggregateYearly = "CFS".equalsIgnoreCase(primarySeries[0].getUnits());
				calcTAFforCFS(primarySeries[0]);
			}
			TimeSeriesContainer tsc = primarySeries[0];
			if("STORAGE-CHANGE".equalsIgnoreCase(tsc.getParameterName()) && "TAF".equalsIgnoreCase(tsc.getUnits()))
			{
				aggregateYearly = true;
			}
		}
		return aggregateYearly;
	}

	private void calcTAFforCFS(TimeSeriesContainer series)
	{
		try
		{
			if("CFS".equalsIgnoreCase(series.getUnits()))
			{
				HecTime ht = new HecTime();
				Calendar calendar = Calendar.getInstance();
				// Primary series
				for(int j = 0; j < series.numberValues; j++)
				{

					ht.set(series.times[j]);
					calendar.set(ht.year(), ht.month() - 1, 1);
					double monthlyTAF = series.values[j]
							* calendar.getActualMaximum(Calendar.DAY_OF_MONTH) * CFS_2_TAF_DAY;
					series.values[j] = monthlyTAF;
				}
			}
		}
		catch(RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to calculate TAF.", ex);
		}
	}

	private SortedMap<WaterYearPeriod, Double> groupWaterYearPeriod(EpptScenarioRun epptScenarioRun,
																	NavigableMap<Integer, Double> filteredYearlyPeriod)
			throws EpptInitializationException
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
				LocalDateTime startYearMonth = yearMonths.get(0).minusMonths(1).atEndOfMonth().minusDays(2).atTime(0, 0);
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

	private WaterYearIndex getWaterYearIndexForScenario(EpptScenarioRun epptScenarioRun) throws EpptInitializationException
	{
		return _selectedIndicies.get(epptScenarioRun);
	}

	private List<WaterYearIndex> getWaterYearIndicesForScenario(EpptScenarioRun epptScenarioRun) throws EpptInitializationException
	{
		return _allIndicies.get(epptScenarioRun);
	}

	final DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, boolean isCFS, LocalDate start, LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = new DSSGrabber1SvcImpl();
		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		dssGrabber.setGuiLink(_guiLink);
		dssGrabber.setDateRange(start, end);
		return dssGrabber;
	}
}

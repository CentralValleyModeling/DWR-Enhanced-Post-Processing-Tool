/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.trendreporting;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.YearMonth;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.OptionalDouble;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.DoubleStream;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearAnnualPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.project.EpptScenarioRun;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import rma.util.RMAConst;

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
	private final GUILinksAllModelsBO _guiLink;
	private final TrendStatistics _statistics;
	private final EpptReportingMonths.MonthPeriod _monthPeriod;
	private final WaterYearDefinition _waterYearDefinition;
	private final WaterYearIndex _waterYearIndex;
	private final List<WaterYearIndex> _waterYearIndices;

	EpptReportingComputer(GUILinksAllModelsBO guiLink, TrendStatistics statistics, EpptReportingMonths.MonthPeriod monthPeriod,
						  WaterYearDefinition waterYearDefinition, WaterYearIndex waterYearIndex, List<WaterYearIndex> waterYearIndices)
	{
		_guiLink = guiLink;
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_waterYearDefinition = waterYearDefinition;
		_waterYearIndex = waterYearIndex;
		_waterYearIndices = waterYearIndices;
	}

	EpptReportingComputed computeCfs(EpptScenarioRun scenarioRun, LocalDate start, LocalDate end) throws EpptInitializationException
	{
		DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(scenarioRun, true, start, end);
		return compute(scenarioRun, dssGrabber, false);
	}

	EpptReportingComputed computeTaf(EpptScenarioRun scenarioRun, LocalDate start, LocalDate end) throws EpptInitializationException
	{
		DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(scenarioRun, false, start, end);
		return compute(scenarioRun, dssGrabber, true);
	}

	private EpptReportingComputed compute(EpptScenarioRun scenarioRun, DSSGrabber1SvcImpl dssGrabber, boolean convertTaf)
			throws EpptInitializationException
	{
		int offset = (int) TimeUnit.MILLISECONDS.toMinutes(TimeZone.getDefault().getRawOffset());
		NavigableMap<LocalDateTime, Double> fullSeries = new TreeMap<>();
		TimeSeriesContainer[] primarySeries = dssGrabber.getPrimarySeries();
		String units = "";
		boolean aggregateYearly = false;
		if(primarySeries != null && primarySeries.length > 0 && primarySeries[0] != null)
		{
			if(convertTaf)
			{
				aggregateYearly = "CFS".equalsIgnoreCase(primarySeries[0].getUnits());
				dssGrabber.calcTAFforCFS(primarySeries, null);
			}
			TimeSeriesContainer tsc = primarySeries[0];
			units = tsc.getParameterName() + " (" + tsc.getUnits() + ")";
			for(int i = 0; i < tsc.getNumberValues(); i++)
			{
				HecTime hecTime = tsc.getHecTime(i);
				double value = tsc.getValue(i);
				if(RMAConst.isValidValue(value))
				{
					Date javaDate = hecTime.getJavaDate(offset);
					fullSeries.put(LocalDateTime.ofInstant(javaDate.toInstant(), ZoneId.systemDefault()), value);
				}
			}
		}
		NavigableMap<Integer, Double> filteredPeriodYearly = filterPeriodYearly(fullSeries, aggregateYearly);
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

	private NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input, boolean aggregateYearly)
	{
		NavigableMap<Integer, Double> retval = new TreeMap<>();
		if(!input.isEmpty())
		{
			int year = input.firstKey().getYear();
			int lastYear = input.lastKey().getYear();
			while(year <= lastYear)
			{
				NavigableMap<LocalDateTime, Double> dataMap = new TreeMap<>();
				List<YearMonth> yearMonths = _monthPeriod.getYearMonths(year);
				if(!yearMonths.isEmpty())
				{
					for(YearMonth yearMonth : yearMonths)
					{
						for(Map.Entry<LocalDateTime, Double> entry : input.entrySet())
						{
							LocalDateTime key = entry.getKey();
							if(key.getMonth() == yearMonth.plusMonths(1).getMonth() && key.getYear() == yearMonth.plusMonths(1).getYear())
							{
								LOGGER.log(Level.FINE, "Value for {0}: {1} YearMonth: {2}",
										new Object[]{year, entry.getValue(), YearMonth.of(key.getYear(), key.getMonth())});
								dataMap.put(entry.getKey(), entry.getValue());
								break;
							}
						}
					}
					Collections.sort(yearMonths);
					if(dataMap.size() == yearMonths.size())
					{
						DoubleStream doubleStream = dataMap.values().stream().mapToDouble(e -> e);
						OptionalDouble rollup;
						if(aggregateYearly)
						{
							rollup = OptionalDouble.of(doubleStream.sum());
						}
						else
						{
							rollup = doubleStream.average();
						}
						Logger.getLogger(EpptReportingComputer.class.getName())
							  .log(Level.FINE, "Average for " + year + ": " + rollup.getAsDouble());
						int yearForOctSepDefinition = year;
						rollup.ifPresent(a -> retval.put(yearForOctSepDefinition, a));
					}
				}
				year++;
			}
		}
		return retval;
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
		List<Month> months = EpptReportingMonths.getMonths(_monthPeriod);

		SortedMap<Month, NavigableMap<Integer, Double>> retval = new TreeMap<>();
		for(Month month : months)
		{
			NavigableMap<Integer, Double> yearlyMap = new TreeMap<>();
			retval.put(month, yearlyMap);
			if(!input.isEmpty())
			{
				int year = input.firstKey().getYear();
				int lastYear = input.lastKey().getYear();
				while(year <= lastYear)
				{
					List<YearMonth> yearMonths = _monthPeriod.getYearMonths(year);
					for(Map.Entry<LocalDateTime, Double> entry : input.entrySet())
					{
						for(YearMonth yearMonth : yearMonths)
						{
							LocalDateTime key = entry.getKey().minusMonths(1);
							Month entryMonth = key.getMonth();
							if(entryMonth == month && key.getYear() == yearMonth.getYear())
							{
								yearlyMap.put(yearMonth.getYear(), entry.getValue());
							}
						}
					}
					year++;
				}
			}
		}
		return retval;
	}

	private WaterYearIndex getWaterYearIndexForScenario(EpptScenarioRun epptScenarioRun) throws EpptInitializationException
	{
		WaterYearTableReader tableReader = new WaterYearTableReader(epptScenarioRun.getWaterYearTable());
		List<WaterYearIndex> read = tableReader.read();
		return read.stream()
				   .filter(index -> _waterYearIndex.getName().equalsIgnoreCase(index.getName()))
				   .findAny()
				   .orElse(_waterYearIndex);
	}

	private List<WaterYearIndex> getWaterYearIndicesForScenario(EpptScenarioRun epptScenarioRun) throws EpptInitializationException
	{
		WaterYearTableReader tableReader = new WaterYearTableReader(epptScenarioRun.getWaterYearTable());
		List<String> collect = _waterYearIndices.stream().map(WaterYearIndex::getName).collect(toList());
		List<WaterYearIndex> read = tableReader.read();
		return read.stream().filter(index -> collect.contains(index.getName())).collect(toList());
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, boolean isCFS, LocalDate start, LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = new DSSGrabber1SvcImpl();
		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		GUILinksAllModelsBO guiLink = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance().getGuiLink(_guiLink.getCheckboxId());
		dssGrabber.setGuiLink(guiLink);
		dssGrabber.setDateRange(start, end);
		return dssGrabber;
	}
}

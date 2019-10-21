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
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
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
public class EpptReportingComputer
{
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

	EpptReportingComputed computeCfs(EpptScenarioRun scenarioRun, LocalDate start, LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(scenarioRun, true, start, end);
		return compute(scenarioRun, dssGrabber, false);
	}

	EpptReportingComputed computeTaf(EpptScenarioRun scenarioRun, LocalDate start, LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(scenarioRun, false, start, end);
		return compute(scenarioRun, dssGrabber, true);
	}

	private EpptReportingComputed compute(EpptScenarioRun scenarioRun, DSSGrabber1SvcImpl dssGrabber, boolean convertTaf)
	{
		int offset = (int) TimeUnit.MILLISECONDS.toMinutes(TimeZone.getDefault().getRawOffset());
		NavigableMap<LocalDateTime, Double> retval = new TreeMap<>();
		TimeSeriesContainer[] primarySeries = dssGrabber.getPrimarySeries();
		String units = "";
		if(primarySeries != null && primarySeries.length > 0 && primarySeries[0] != null)
		{
			if(convertTaf)
			{
				dssGrabber.calcTAFforCFS(primarySeries, null);
			}
			TimeSeriesContainer tsc = primarySeries[0];
			units = tsc.getParameterName() + " (" + tsc.getUnits() +")";
			for(int i = 0; i < tsc.getNumberValues(); i++)
			{
				HecTime hecTime = tsc.getHecTime(i);
				double value = tsc.getValue(i);
				if(RMAConst.isValidValue(value))
				{
					Date javaDate = hecTime.getJavaDate(offset);
					retval.put(LocalDateTime.ofInstant(javaDate.toInstant(), ZoneId.systemDefault()), value);
				}
			}
		}
		Map<LocalDateTime, Double> filteredPeriod = filterPeriod(retval);
		Map<WaterYearPeriod, SortedMap<Month, Double>> waterYearPeriodGrouped = groupWaterYearPeriod(filteredPeriod);
		Map<Month, Double> calculate = _statistics.calculate(filteredPeriod, _waterYearDefinition, _waterYearIndex, _waterYearIndices);
		return new EpptReportingComputed(scenarioRun, retval, filteredPeriod, sort(calculate), waterYearPeriodGrouped, units);
	}

	private Map<WaterYearPeriod, SortedMap<Month, Double>> groupWaterYearPeriod(Map<LocalDateTime, Double> filteredPeriod)
	{
		List<WaterYearPeriod> sortedPeriods = _waterYearIndex.getSortedPeriods();
		Map<WaterYearPeriod, SortedMap<Month, Double>> retval = new TreeMap<>(Comparator.comparingInt(sortedPeriods::indexOf));
		for(WaterYearPeriod waterYearPeriod : sortedPeriods)
		{
			List<WaterYearPeriodRange> periodRange = _waterYearIndex.getWaterYearTypeGroups().getOrDefault(waterYearPeriod, new ArrayList<>())
																	.stream()
																	.map(e->new WaterYearPeriodRange(e.getWaterYearPeriod(), e, e))
																	.collect(toList());
			WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter(periodRange, _waterYearDefinition);
			SortedMap<LocalDateTime, Double> grouped = filteredPeriod.entrySet()
					.stream()
					.filter(waterYearPeriodRangesFilter)
					.collect(toMap(Map.Entry::getKey, Map.Entry::getValue, (o, n)->n, TreeMap::new));
			Map<Month, Double> calculate = _statistics.calculate(grouped, _waterYearDefinition, _waterYearIndex, _waterYearIndices);
			retval.put(waterYearPeriod, sort(calculate));
		}
		return retval;
	}

	private SortedMap<Month, Double> sort(Map<Month, Double> calculate)
	{
		Map<Month, Double> calculateEop = new EnumMap<>(Month.class);
		for(Map.Entry<Month, Double> entry : calculate.entrySet())
		{
			calculateEop.put(entry.getKey().minus(1), entry.getValue());
		}
		List<Month> months = EpptReportingMonths.getMonths(_monthPeriod);
		SortedMap<Month, Double> retval = new TreeMap<>(Comparator.comparingInt(months::indexOf));
		retval.putAll(calculateEop);
		return retval;
	}

	private Map<LocalDateTime, Double> filterPeriod(NavigableMap<LocalDateTime, Double> values)
	{
		List<Month> months = EpptReportingMonths.getMonths(_monthPeriod);
		if(!values.isEmpty())
		{
			LocalDateTime start = values.firstKey();
			LocalDateTime end = values.lastKey();
			Set<LocalDateTime> ascendinding = values.keySet();
			for(LocalDateTime date : ascendinding)
			{
				Month month = date.getMonth().minus(1);
				if(months.get(0) == month)
				{
					start = date;
					break;
				}
			}
			Set<LocalDateTime> descending = values.descendingKeySet();
			for(LocalDateTime date : descending)
			{
				Month month = date.getMonth().minus(1);
				if(months.get(months.size() - 1) == month)
				{
					end = date;
					break;
				}
			}
			values = values.subMap(start, true, end, true);
		}
		return values.entrySet()
					 .stream()
					 .filter(e -> months.contains(e.getKey().getMonth().minus(1)))
					 .collect(toMap(Map.Entry::getKey, Map.Entry::getValue));
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, boolean isCFS, LocalDate start, LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = new DSSGrabber1SvcImpl();
		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		dssGrabber.setLocation(_guiLink.getCheckboxId());
		dssGrabber.setDateRange(start, end);
		return dssGrabber;
	}
}

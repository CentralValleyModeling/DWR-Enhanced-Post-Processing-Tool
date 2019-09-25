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
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.project.EpptScenarioRun;

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
	private final GUILinksAllModelsBO _guiLink;
	private final TrendStatistics _statistics;
	private final EpptReportingMonths.MonthPeriod _monthPeriod;
	private final WaterYearDefinition _waterYearDefinition;
	private final WaterYearIndex _waterYearIndex;

	public EpptReportingComputer(GUILinksAllModelsBO guiLink, TrendStatistics statistics, EpptReportingMonths.MonthPeriod monthPeriod,
								 WaterYearDefinition waterYearDefinition, WaterYearIndex waterYearIndex)
	{
		_guiLink = guiLink;
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_waterYearDefinition = waterYearDefinition;
		_waterYearIndex = waterYearIndex;
	}

	public EpptReportingComputed computeCfs(EpptScenarioRun scenarioRun, LocalDate start, LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(scenarioRun, true, start, end);
		return compute(scenarioRun, dssGrabber);
	}

	public EpptReportingComputed computeTaf(EpptScenarioRun scenarioRun, LocalDate start, LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(scenarioRun, false, start, end);
		return compute(scenarioRun, dssGrabber);
	}

	private EpptReportingComputed compute(EpptScenarioRun scenarioRun, DSSGrabber1SvcImpl dssGrabber)
	{
		int offset = (int) TimeUnit.MILLISECONDS.toMinutes(TimeZone.getDefault().getRawOffset());
		Map<LocalDateTime, Double> retval = new HashMap<>();
		TimeSeriesContainer[] primarySeries = dssGrabber.getPrimarySeries();
		String units = "";
		if(primarySeries != null && primarySeries.length > 0 && primarySeries[0] != null)
		{
			TimeSeriesContainer tsc = primarySeries[0];
			units = tsc.getUnits();
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
		Map<Month, Double> calculate = _statistics.calculate(filteredPeriod, _waterYearDefinition, _waterYearIndex);
		Map<Month, Double> calculateEop = new EnumMap<>(Month.class);
		for(Map.Entry<Month, Double> entry : calculate.entrySet())
		{
			calculateEop.put(entry.getKey().minus(1), entry.getValue());
		}
		return new EpptReportingComputed(scenarioRun, retval, filteredPeriod, sort(calculateEop), units);
	}

	private SortedMap<Month, Double> sort(Map<Month, Double> calculate)
	{
		List<Month> months = EpptReportingMonths.getMonths(_monthPeriod);
		SortedMap<Month, Double> retval = new TreeMap<>(Comparator.comparingInt(months::indexOf));
		retval.putAll(calculate);
		return retval;
	}

	private Map<LocalDateTime, Double> filterPeriod(Map<LocalDateTime, Double> values)
	{
		List<Month> months = EpptReportingMonths.getMonths(_monthPeriod);
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

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

package gov.ca.water.calgui.scripts;

import java.time.LocalDateTime;
import java.time.Month;
import java.time.YearMonth;
import java.time.ZoneId;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.DoubleStream;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ThresholdLinksBO;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.DetailedIssuesReader;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.busservice.impl.ThresholdLinksSeedDataSvc;
import gov.ca.water.calgui.project.EpptScenarioRun;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import hec.lang.annotation.Scriptable;
import rma.util.RMAConst;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-14-2019
 */
public class DssReader
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private final EpptScenarioRun _scenarioRun;
	private final LocalDateTime _start;
	private final LocalDateTime _end;
	private String _units;

	public DssReader(EpptScenarioRun scenarioRun, LocalDateTime start, LocalDateTime end)
	{
		_scenarioRun = scenarioRun;
		_start = start;
		_end = end;
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyGuiLinkData(int guiID, Month startMonth) throws DssMissingRecordException
	{
		return filterPeriodYearly(getGuiLinkData(guiID, true), startMonth);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyGuiLinkData(int guiID, boolean mapToTaf, Month startMonth) throws DssMissingRecordException
	{
		return filterPeriodYearly(getGuiLinkData(guiID, mapToTaf), startMonth);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyDtsData(int dtsId, Month startMonth) throws DssMissingRecordException
	{
		return filterPeriodYearly(getDtsData(dtsId, true), startMonth);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyDtsData(int dtsId, boolean mapToTaf, Month startMonth) throws DssMissingRecordException
	{
		return filterPeriodYearly(getDtsData(dtsId, mapToTaf), startMonth);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyThresholdData(int dtsId, Month startMonth) throws DssMissingRecordException
	{
		return filterPeriodYearly(getThresholdData(dtsId, true), startMonth);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyThresholdData(int dtsId, boolean mapToTaf, Month startMonth) throws DssMissingRecordException
	{
		return filterPeriodYearly(getThresholdData(dtsId, mapToTaf), startMonth);
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getGuiLinkData(int guiID) throws DssMissingRecordException
	{
		return getGuiLinkData(guiID, true);
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getGuiLinkData(int guiID, boolean mapToTaf) throws DssMissingRecordException
	{
		DssCache instance = DssCache.getInstance();
		NavigableMap<LocalDateTime, Double> retval = instance.readGuiLinkFromCache(_scenarioRun, guiID);
		_units = instance.readGuiLinkUnitsFromCache(_scenarioRun, guiID);
		if(retval == null)
		{
			try
			{
				GUILinksAllModelsBO guiLink = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance().getGuiLink(Integer.toString(guiID));
				DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(_scenarioRun, guiLink);
				TimeSeriesContainer[] primarySeries = dssGrabber1Svc.getPrimarySeries();
				if(mapToTaf)
				{
					primarySeries = mapToTaf(primarySeries, dssGrabber1Svc);
				}
				if(primarySeries == null || primarySeries[0] == null)
				{
					throw new DssMissingRecordException(_scenarioRun.getName() + ": Unable to find matching GUILink with ID: " + guiID);
				}
				retval = timeSeriesContainerToMap(primarySeries);
				instance.addGuiLinkToCache(_scenarioRun, guiID, retval);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading GUI Links Data: " + guiID);
				retval = new TreeMap<>();
			}
		}
		instance.addGuiLinkUnitsToCache(_scenarioRun, guiID, _units);
		return retval;
	}

	private TimeSeriesContainer[] mapToTaf(TimeSeriesContainer[] primarySeries, DSSGrabber1SvcImpl dssGrabber1Svc)
	{
		if(primarySeries != null && primarySeries[0] != null)
		{
			dssGrabber1Svc.calcTAFforCFS(primarySeries, null);
		}
		return primarySeries;
	}

	private NavigableMap<LocalDateTime, Double> timeSeriesContainerToMap(TimeSeriesContainer[] primarySeries)
	{
		NavigableMap<LocalDateTime, Double> retval = new TreeMap<>();
		if(primarySeries != null && primarySeries[0] != null)
		{
			TimeSeriesContainer tsc = primarySeries[0];
			if(tsc.times != null)
			{
				String units = tsc.getUnits();
				LOGGER.at(Level.FINER).log("Timeseries %s units are %s", tsc.getShortName(), units);
				_units = units;
				for(int i = 0; i < tsc.times.length; i++)
				{
					HecTime hecTime = new HecTime();
					hecTime.set(tsc.times[i], tsc.timeGranularitySeconds, tsc.julianBaseDate);
					double value = tsc.getValue(i);
					int offset = (int) TimeUnit.MILLISECONDS.toMinutes(TimeZone.getDefault().getRawOffset());
					Date javaDate = hecTime.getJavaDate(offset);
					LocalDateTime localDateTime = LocalDateTime.ofInstant(javaDate.toInstant(), ZoneId.systemDefault());
					if(RMAConst.isValidValue(value))
					{
						if(tsc.getParameterName().toLowerCase().contains("percent"))
						{
							value *= 100;
						}
						retval.put(localDateTime, value);
					}
					else
					{
						retval.put(localDateTime, Double.NaN);
						LOGGER.at(Level.FINE).log("Invalid value %d found at: %s", localDateTime);
					}
				}
			}
		}
		return retval;
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, ThresholdLinksBO thresholdLink)
	{
		DSSGrabber1SvcImpl grabber1Svc = new DSSGrabber1SvcImpl();
		grabber1Svc.setDateRange(_start.toLocalDate(), _end.toLocalDate());
		grabber1Svc.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		grabber1Svc.setThresholdLink(thresholdLink);
		grabber1Svc.setIsCFS(false);
		return grabber1Svc;
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, DetailedIssue dtsLink)
	{
		DSSGrabber1SvcImpl grabber1Svc = new DSSGrabber1SvcImpl();
		grabber1Svc.setDateRange(_start.toLocalDate(), _end.toLocalDate());
		grabber1Svc.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		grabber1Svc.setDtsLink(dtsLink);
		grabber1Svc.setIsCFS(false);
		return grabber1Svc;
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, GUILinksAllModelsBO guiLink)
	{
		DSSGrabber1SvcImpl grabber1Svc = new DSSGrabber1SvcImpl();
		grabber1Svc.setDateRange(_start.toLocalDate(), _end.toLocalDate());
		grabber1Svc.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		grabber1Svc.setGuiLink(guiLink);
		grabber1Svc.setIsCFS(false);
		return grabber1Svc;
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getDtsData(int dtsId) throws DssMissingRecordException
	{
		return getDtsData(dtsId, false);
	}

	@SuppressWarnings("unchecked")
	@Scriptable
	public NavigableMap<LocalDateTime, Double> getDtsData(int dtsId, boolean mapToTaf) throws DssMissingRecordException
	{
		DssCache instance = DssCache.getInstance();
		NavigableMap<LocalDateTime, Double> retval = instance.readDtsLinkFromCache(_scenarioRun, dtsId);
		_units = instance.readDtsLinkUnitsFromCache(_scenarioRun, dtsId);
		if(retval == null)
		{
			try
			{
				Optional<DetailedIssue> issue = DetailedIssuesReader.getInstance().getDetailedIssues()
																	.stream()
																	.filter(di -> di.getDetailedIssueId() == dtsId)
																	.findAny();
				DetailedIssue detailedIssue = issue.orElseThrow(
						() -> new DssMissingRecordException(_scenarioRun.getName() + ": Unable to find matching DTS path for DTS ID: " + dtsId));
				DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(_scenarioRun, detailedIssue);
				TimeSeriesContainer[] primarySeries = dssGrabber1Svc.getPrimarySeries();
				if(mapToTaf)
				{
					primarySeries = mapToTaf(primarySeries, dssGrabber1Svc);
				}
				if(primarySeries == null || primarySeries[0] == null)
				{
					throw new DssMissingRecordException(_scenarioRun.getName() +
							": Unable to find matching DTS record with DTS ID: " + dtsId + " and DSS Path: " + detailedIssue.getLinkedVar());
				}
				retval = timeSeriesContainerToMap(primarySeries);
				instance.addDtsLinkToCache(_scenarioRun, dtsId, retval);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading GUI Links Data: " + dtsId);
				retval = new TreeMap<>();
			}
		}
		instance.addDtsLinkUnitsToCache(_scenarioRun, dtsId, _units);
		return retval;
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getThresholdData(int thresholdId) throws DssMissingRecordException
	{
		return getThresholdData(thresholdId, true);
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getThresholdData(int thresholdId, boolean mapToTaf) throws DssMissingRecordException
	{
		DssCache instance = DssCache.getInstance();
		NavigableMap<LocalDateTime, Double> retval = instance.readThresholdLinkFromCache(_scenarioRun, thresholdId);
		_units = instance.readThresholdLinkUnitsFromCache(_scenarioRun, thresholdId);
		if(retval == null)
		{
			try
			{
				ThresholdLinksBO thresholdLink = ThresholdLinksSeedDataSvc.getSeedDataSvcImplInstance().getObjById(thresholdId);
				DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(_scenarioRun, thresholdLink);
				TimeSeriesContainer[] threshold = dssGrabber1Svc.getPrimarySeries();
				if(mapToTaf)
				{
					threshold = mapToTaf(threshold, dssGrabber1Svc);
				}
				if(threshold == null || threshold[0] == null)
				{
					throw new DssMissingRecordException(_scenarioRun.getName() +
							": Unable to find matching Threshold path for B-Part: " + thresholdLink.getModelData(
							GUILinksAllModelsBO.Model.values().get(0)) + " and ID: " + thresholdLink.getId());
				}
				retval = timeSeriesContainerToMap(threshold);
				instance.addThresholdLinkToCache(_scenarioRun, thresholdId, retval);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading Threshold Links Data: " + thresholdId);
				retval = new TreeMap<>();

			}
		}
		instance.addThresholdLinkUnitsToCache(_scenarioRun, thresholdId, _units);
		return retval;
	}
	public NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input,
																   Month startMonth)
	{
		MonthPeriod monthPeriod = new MonthPeriod(startMonth, startMonth.minus(1L));
		boolean aggregateYearly = "CFS".equalsIgnoreCase(getUnits());
		return filterPeriodYearly(input, monthPeriod, aggregateYearly);
	}

	@Scriptable
	public String getUnits()
	{
		return _units;
	}


	public static NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input,
																   MonthPeriod monthPeriod,
																   boolean aggregateYearly)
	{
		NavigableMap<Integer, Double> retval = new TreeMap<>();
		if(!input.isEmpty())
		{
			int year = input.firstKey().getYear();
			int lastYear = input.lastKey().getYear();
			while(year <= lastYear)
			{
				NavigableMap<LocalDateTime, Double> dataMap = new TreeMap<>();
				List<YearMonth> yearMonths = monthPeriod.getYearMonths(year);
				if(!yearMonths.isEmpty())
				{
					for(YearMonth yearMonth : yearMonths)
					{
						for(Map.Entry<LocalDateTime, Double> entry : input.entrySet())
						{
							LocalDateTime key = entry.getKey();
							if(key.getMonth() == yearMonth.plusMonths(1).getMonth() && key.getYear() == yearMonth.plusMonths(1).getYear())
							{
								LOGGER.at(Level.FINE)
									  .log("Value for %s: %s YearMonth: %s", year, entry.getValue(), YearMonth.of(key.getYear(), key.getMonth()));
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
						LOGGER.at(Level.FINE).log("Average for %s : ", year, rollup.orElse(Double.NaN));
						int yearForOctSepDefinition = year;
						rollup.ifPresent(a -> retval.put(yearForOctSepDefinition, a));
					}
				}
				year++;
			}
		}
		return retval;
	}
}

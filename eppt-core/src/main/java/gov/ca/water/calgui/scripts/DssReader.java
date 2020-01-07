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

import java.time.LocalDate;
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
import java.util.stream.DoubleStream;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.ThresholdLinksBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
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

import static java.util.stream.Collectors.toMap;

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
	private final WaterYearDefinition _waterYearDefinition;
	private String _units;
	private String _originalUnits;
	private String _parameter;

	public DssReader(EpptScenarioRun scenarioRun, WaterYearDefinition waterYearDefinition)
	{
		_scenarioRun = scenarioRun;
		_waterYearDefinition = waterYearDefinition;
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyGuiLinkData(int guiID) throws DssMissingRecordException
	{
		return filterPeriodYearly(getGuiLinkData(guiID, true), _waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyGuiLinkData(int guiID, WaterYearDefinition waterYearDefinition) throws DssMissingRecordException
	{
		return filterPeriodYearly(getGuiLinkData(guiID, true), waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyGuiLinkData(int guiID, boolean mapToTaf, WaterYearDefinition waterYearDefinition)
			throws DssMissingRecordException
	{
		return filterPeriodYearly(getGuiLinkData(guiID, mapToTaf), waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyDtsData(int dtsId) throws DssMissingRecordException
	{
		return filterPeriodYearly(getDtsData(dtsId, true), _waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyDtsData(int dtsId, WaterYearDefinition waterYearDefinition) throws DssMissingRecordException
	{
		return filterPeriodYearly(getDtsData(dtsId, true), waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyDtsData(int dtsId, boolean mapToTaf, WaterYearDefinition waterYearDefinition)
			throws DssMissingRecordException
	{
		return filterPeriodYearly(getDtsData(dtsId, mapToTaf), waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyThresholdData(int dtsId) throws DssMissingRecordException
	{
		return filterPeriodYearly(getThresholdData(dtsId, true), _waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyThresholdData(int dtsId, WaterYearDefinition waterYearDefinition) throws DssMissingRecordException
	{
		return filterPeriodYearly(getThresholdData(dtsId, true), waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyThresholdData(int dtsId, boolean mapToTaf, WaterYearDefinition waterYearDefinition)
			throws DssMissingRecordException
	{
		return filterPeriodYearly(getThresholdData(dtsId, mapToTaf), waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getGuiLinkData(int guiID) throws DssMissingRecordException
	{
		return getGuiLinkData(guiID, true);
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getGuiLinkData(int guiID, boolean mapToTaf) throws DssMissingRecordException
	{
		NavigableMap<LocalDateTime, Double> retval = new TreeMap<>();
		DssCache instance = DssCache.getInstance();
		DssCache.TSValue tsc = instance.readGuiLinkFromCache(_scenarioRun, guiID);
		if(tsc == null)
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
				_originalUnits = dssGrabber1Svc.getOriginalUnits();
				_parameter = primarySeries[0].getParameterName();
				retval = timeSeriesContainerToMap(primarySeries);
				instance.addGuiLinkToCache(_scenarioRun, guiID, primarySeries[0], _originalUnits);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading GUI Links Data: " + guiID);
				retval = new TreeMap<>();
			}
		}
		else
		{
			retval = timeSeriesContainerToMap(new TimeSeriesContainer[]{tsc.getTsc()});
			_units = tsc.getTsc().getUnits();
			_originalUnits = tsc.getOriginalUnits();
			_parameter = tsc.getTsc().getParameterName();
		}
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
		grabber1Svc.setDateRange(LocalDate.of(1800, 1, 1), LocalDate.of(2200, 1, 1));
		grabber1Svc.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		grabber1Svc.setThresholdLink(thresholdLink);
		grabber1Svc.setIsCFS(false);
		return grabber1Svc;
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, DetailedIssue dtsLink)
	{
		DSSGrabber1SvcImpl grabber1Svc = new DSSGrabber1SvcImpl();
		grabber1Svc.setDateRange(LocalDate.of(1800, 1, 1), LocalDate.of(2200, 1, 1));
		grabber1Svc.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		grabber1Svc.setDtsLink(dtsLink);
		grabber1Svc.setIsCFS(false);
		return grabber1Svc;
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, GUILinksAllModelsBO guiLink)
	{
		DSSGrabber1SvcImpl grabber1Svc = new DSSGrabber1SvcImpl();
		grabber1Svc.setDateRange(LocalDate.of(1800, 1, 1), LocalDate.of(2200, 1, 1));
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
		NavigableMap<LocalDateTime, Double> retval = new TreeMap<>();
		DssCache instance = DssCache.getInstance();
		DssCache.TSValue tsc = instance.readDtsLinkFromCache(_scenarioRun, dtsId);
		if(tsc == null)
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
				_originalUnits = dssGrabber1Svc.getOriginalUnits();
				_parameter = primarySeries[0].getParameterName();
				retval = timeSeriesContainerToMap(primarySeries);
				instance.addDtsLinkToCache(_scenarioRun, dtsId, primarySeries[0], _originalUnits);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading GUI Links Data: " + dtsId);
				retval = new TreeMap<>();
			}
		}
		else
		{
			retval = timeSeriesContainerToMap(new TimeSeriesContainer[]{tsc.getTsc()});
			_units = tsc.getTsc().getUnits();
			_originalUnits = tsc.getOriginalUnits();
			_parameter = tsc.getTsc().getParameterName();
		}
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
		NavigableMap<LocalDateTime, Double> retval = new TreeMap<>();
		DssCache instance = DssCache.getInstance();
		DssCache.TSValue tsc = instance.readThresholdLinkFromCache(_scenarioRun, thresholdId);
		if(tsc == null)
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
				_originalUnits = dssGrabber1Svc.getOriginalUnits();
				_parameter = threshold[0].getParameterName();
				retval = timeSeriesContainerToMap(threshold);
				instance.addThresholdLinkToCache(_scenarioRun, thresholdId, threshold[0], _originalUnits);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading Threshold Links Data: " + thresholdId);
				retval = new TreeMap<>();

			}
		}
		else
		{
			retval = timeSeriesContainerToMap(new TimeSeriesContainer[]{tsc.getTsc()});
			_units = tsc.getTsc().getUnits();
			_originalUnits = tsc.getOriginalUnits();
			_parameter = tsc.getTsc().getParameterName();
		}
		return retval;
	}

	private NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input,
															 WaterYearDefinition waterYearDefinition)
	{
		MonthPeriod monthPeriod = new MonthPeriod(waterYearDefinition.getStartMonth(), waterYearDefinition.getEndMonth());
		boolean aggregateYearly = "CFS".equalsIgnoreCase(_originalUnits)
				|| "STORAGE-CHANGE".equalsIgnoreCase(_parameter) && "TAF".equalsIgnoreCase(getUnits());
		return filterPeriodYearly(input, monthPeriod, aggregateYearly);
	}

	public NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input,
															Month startMonth)
	{
		return filterPeriodYearly(input, new WaterYearDefinition("Yearly", startMonth, startMonth.minus(1)));
	}

	@Scriptable
	public String getUnits()
	{
		return _units;
	}

	@Scriptable
	public NavigableMap<Integer, Double> aggregateYearly(NavigableMap<LocalDateTime, Double> input)
	{
		MonthPeriod monthPeriod = new MonthPeriod(_waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth());
		NavigableMap<Integer, Double> retval = filterPeriodYearly(input, monthPeriod, true);
		return retval;
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

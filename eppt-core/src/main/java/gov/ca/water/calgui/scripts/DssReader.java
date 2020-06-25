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
import java.util.Calendar;
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
import gov.ca.water.calgui.busservice.impl.ErrorValueFlags;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.busservice.impl.ThresholdLinksSeedDataSvc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import hec.lang.annotation.Scriptable;
import rma.util.RMAConst;

import static gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl.CFS_2_TAF_DAY;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-14-2019
 */
@SuppressWarnings({"unused", "WeakerAccess"})
public class DssReader
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private final EpptScenarioRun _scenarioRun;
	private final WaterYearDefinition _waterYearDefinition;
	private final DssCache _dssCache;
	private String _units;
	private String _originalUnits;
	private String _parameter;

	public DssReader(EpptScenarioRun scenarioRun, WaterYearDefinition waterYearDefinition, DssCache dssCache)
	{
		_scenarioRun = scenarioRun;
		_waterYearDefinition = waterYearDefinition;
		_dssCache = dssCache;
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyGuiLinkData(int guiID) throws DssMissingRecordException
	{
		return filterPeriodYearly(getGuiLinkData(guiID, true), _waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyGuiLinkData(int guiID, Month month) throws DssMissingRecordException
	{
		WaterYearDefinition waterYearDefinition = new WaterYearDefinition("", month, month.minus(1), 1, 1);
		return filterPeriodYearly(getGuiLinkData(guiID, true), waterYearDefinition);
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
		return filterPeriodYearly(getGuiLinkData(guiID, mapToTaf), waterYearDefinition, mapToTaf);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyDtsData(int dtsId) throws DssMissingRecordException
	{
		return filterPeriodYearly(getDtsData(dtsId, true), _waterYearDefinition);
	}

	@Scriptable
	public NavigableMap<Integer, Double> getYearlyDtsData(int dtsId, Month month) throws DssMissingRecordException
	{
		WaterYearDefinition waterYearDefinition = new WaterYearDefinition("", month, month.minus(1), 1, 1);
		return filterPeriodYearly(getDtsData(dtsId, true), waterYearDefinition);
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
		return filterPeriodYearly(getDtsData(dtsId, mapToTaf), waterYearDefinition, mapToTaf);
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
		return filterPeriodYearly(getThresholdData(dtsId, mapToTaf), waterYearDefinition, mapToTaf);
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getGuiLinkData(int guiID) throws DssMissingRecordException
	{
		return getGuiLinkData(guiID, true);
	}

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getGuiLinkData(int guiID, boolean mapToTaf) throws DssMissingRecordException
	{
		NavigableMap<LocalDateTime, Double> retval;
		DssCache.TSValue tsc = _dssCache.readGuiLinkFromCache(_scenarioRun, guiID);
		if(tsc == null)
		{
			try
			{
				GUILinksAllModelsBO guiLink = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance().getGuiLink(Integer.toString(guiID));
				DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(_scenarioRun, guiLink);
				TimeSeriesContainer[] primarySeries = dssGrabber1Svc.getPrimarySeries();
				if(primarySeries == null || primarySeries[0] == null)
				{
					throw new DssMissingRecordException(_scenarioRun.getName() + ": Unable to find matching GUILink with ID: " + guiID);
				}
				_originalUnits = dssGrabber1Svc.getOriginalUnits();
				_parameter = primarySeries[0].getParameterName();
				retval = timeSeriesContainerToMap(primarySeries, mapToTaf);
				_dssCache.addGuiLinkToCache(_scenarioRun, guiID, primarySeries[0], _originalUnits);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading GUI Links Data: " + guiID);
				retval = new TreeMap<>();
			}
		}
		else
		{
			_units = tsc.getTsc().getUnits();
			_originalUnits = tsc.getOriginalUnits();
			_parameter = tsc.getTsc().getParameterName();
			retval = timeSeriesContainerToMap(new TimeSeriesContainer[]{tsc.getTsc()}, mapToTaf);
		}
		return retval;
	}

	public NavigableMap<LocalDateTime, Double> timeSeriesContainerToMap(TimeSeriesContainer[] primarySeries, boolean mapToTaf)
	{
		NavigableMap<LocalDateTime, Double> retval = new TreeMap<>();
		if(primarySeries != null && primarySeries[0] != null)
		{
			TimeSeriesContainer tsc = primarySeries[0];
			if(tsc.times != null)
			{
				Calendar calendar = Calendar.getInstance();
				for(int i = 0; i < tsc.times.length; i++)
				{
					double value = tsc.getValue(i);
					HecTime hecTime = new HecTime();
					hecTime.set(tsc.times[i], tsc.timeGranularitySeconds, tsc.julianBaseDate);
					addValueToMap(retval, tsc, mapToTaf, calendar, value, hecTime);
				}
				String units = tsc.getUnits();
				LOGGER.at(Level.FINER).log("Timeseries %s units are %s", tsc.getShortName(), units);
				_units = units;
			}
		}
		return retval;
	}

	private void addValueToMap(NavigableMap<LocalDateTime, Double> retval, TimeSeriesContainer tsc, boolean mapToTaf,
							   Calendar calendar, double value, HecTime hecTime)
	{
		int offset = (int) TimeUnit.MILLISECONDS.toMinutes(TimeZone.getDefault().getRawOffset());
		Date javaDate = hecTime.getJavaDate(offset);
		LocalDateTime localDateTime = LocalDateTime.ofInstant(javaDate.toInstant(), ZoneId.systemDefault());
		if(RMAConst.isValidValue(value) && !ErrorValueFlags.isErrorValue(value))
		{
			if(tsc.getParameterName().toLowerCase().contains("percent")
					|| tsc.getUnits().toLowerCase().contains("percent")
					|| tsc.getUnits().toLowerCase().contains("%"))
			{
				value *= 100;
			}
			if(mapToTaf && "CFS".equalsIgnoreCase(_originalUnits))
			{
				tsc.setUnits("TAF");
				calendar.set(hecTime.year(), hecTime.month() - 1, 1);
				value *= calendar.getActualMaximum(Calendar.DAY_OF_MONTH) * CFS_2_TAF_DAY;
			}
			retval.put(localDateTime, value);
		}
		else
		{
			retval.put(localDateTime, Double.NaN);
			LOGGER.at(Level.FINE).log("Invalid value %d found at: %s", localDateTime);
		}
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

	@Scriptable
	public NavigableMap<LocalDateTime, Double> getDtsData(int dtsId, boolean mapToTaf) throws DssMissingRecordException
	{
		NavigableMap<LocalDateTime, Double> retval;
		DssCache.TSValue tsc = _dssCache.readDtsLinkFromCache(_scenarioRun, dtsId);
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
				if(primarySeries == null || primarySeries[0] == null)
				{
					throw new DssMissingRecordException(_scenarioRun.getName() +
							": Unable to find matching DTS record with DTS ID: " + dtsId + " and DSS Path: " + detailedIssue.getLinkedVar());
				}
				_originalUnits = dssGrabber1Svc.getOriginalUnits();
				_parameter = primarySeries[0].getParameterName();
				retval = timeSeriesContainerToMap(primarySeries, mapToTaf);
				_dssCache.addDtsLinkToCache(_scenarioRun, dtsId, primarySeries[0], _originalUnits);
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading GUI Links Data: " + dtsId);
				retval = new TreeMap<>();
			}
		}
		else
		{
			_units = tsc.getTsc().getUnits();
			_originalUnits = tsc.getOriginalUnits();
			_parameter = tsc.getTsc().getParameterName();
			retval = timeSeriesContainerToMap(new TimeSeriesContainer[]{tsc.getTsc()}, mapToTaf);
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
		NavigableMap<LocalDateTime, Double> retval;
		DssCache.TSValue tsc = _dssCache.readThresholdLinkFromCache(_scenarioRun, thresholdId);
		if(tsc == null)
		{
			try
			{
				ThresholdLinksBO thresholdLink = ThresholdLinksSeedDataSvc.getSeedDataSvcImplInstance().getObjById(thresholdId);
				if(thresholdLink != null)
				{

					DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(_scenarioRun, thresholdLink);
					TimeSeriesContainer[] threshold = dssGrabber1Svc.getPrimarySeries();
					if(threshold == null || threshold[0] == null)
					{
						throw new DssMissingRecordException(_scenarioRun.getName() +
								": Unable to find matching Threshold path for B/C-Part: " + thresholdLink.getModelData(
								GUILinksAllModelsBO.Model.values().get(0)).getPrimary() + " and ID: " + thresholdLink.getId());
					}
					_originalUnits = dssGrabber1Svc.getOriginalUnits();
					_parameter = threshold[0].getParameterName();
					retval = timeSeriesContainerToMap(threshold, mapToTaf);
					_dssCache.addThresholdLinkToCache(_scenarioRun, thresholdId, threshold[0], _originalUnits);
				}
				else
				{
					throw new DssMissingRecordException(_scenarioRun.getName() +
							": Unable to find matching Threshold link for ID " + thresholdId);
				}
			}
			catch(RuntimeException e)
			{
				LOGGER.atSevere().withCause(e).log("Error reading Threshold Links Data: " + thresholdId);
				retval = new TreeMap<>();

			}
		}
		else
		{
			_units = tsc.getTsc().getUnits();
			_originalUnits = tsc.getOriginalUnits();
			_parameter = tsc.getTsc().getParameterName();
			retval = timeSeriesContainerToMap(new TimeSeriesContainer[]{tsc.getTsc()}, mapToTaf);
		}
		return retval;
	}


	private NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input,
															 WaterYearDefinition waterYearDefinition)
	{
		return filterPeriodYearly(input, waterYearDefinition, true);
	}

	private NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input,
															 WaterYearDefinition waterYearDefinition, boolean convertTaf)
	{

		MonthPeriod monthPeriod = new MonthPeriod("", waterYearDefinition.getStartMonth(), waterYearDefinition.getEndMonth());
		boolean aggregateYearly = Constant.isAggregateYearly(convertTaf, _parameter, _originalUnits);
		return filterPeriodYearly(input, monthPeriod, aggregateYearly);
	}

	public NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input,
															Month startMonth)
	{
		return filterPeriodYearly(input, new WaterYearDefinition("Yearly", startMonth, startMonth.minus(1), 1, 1));
	}

	@Scriptable
	public String getUnits()
	{
		return _units;
	}

	@Scriptable
	public NavigableMap<Integer, Double> aggregateYearly(NavigableMap<LocalDateTime, Double> input)
	{
		MonthPeriod monthPeriod = new MonthPeriod("", _waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth());
		return filterPeriodYearly(input, monthPeriod, true);
	}

	public static NavigableMap<Integer, Double> filterPeriodYearly(NavigableMap<LocalDateTime, Double> input,
																   MonthPeriod monthPeriod,
																   boolean aggregateYearly)
	{
		NavigableMap<Integer, Double> retval = new TreeMap<>();
		if(input.isEmpty())
		{
			return retval;
		}
		int year = input.firstKey().getYear();
		int lastYear = input.lastKey().getYear();
		while(year <= lastYear)
		{
			List<YearMonth> yearMonths = monthPeriod.getYearMonths(year);
			if(!yearMonths.isEmpty())
			{
				NavigableMap<LocalDateTime, Double> dataMap = buildDataMap(input, year, yearMonths);
				Collections.sort(yearMonths);
				if(dataMap.size() == yearMonths.size())
				{
					DoubleStream doubleStream = dataMap.values().stream().mapToDouble(e -> e).filter(d -> !Double.isNaN(d));
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
		return retval;
	}

	private static NavigableMap<LocalDateTime, Double> buildDataMap(NavigableMap<LocalDateTime, Double> input, int year, List<YearMonth> yearMonths)
	{
		NavigableMap<LocalDateTime, Double> dataMap = new TreeMap<>();
		for(YearMonth yearMonth : yearMonths)
		{
			for(Map.Entry<LocalDateTime, Double> entry : input.entrySet())
			{
				LocalDateTime key = entry.getKey();
				if(key.minusMonths(1).getMonth() == yearMonth.getMonth() && key.minusMonths(1).getYear() == yearMonth.getYear())
				{
					LOGGER.at(Level.FINE)
						  .log("Value for %s: %s YearMonth: %s", year, entry.getValue(), YearMonth.of(key.getYear(), key.getMonth()));
					dataMap.put(entry.getKey(), entry.getValue());
					break;
				}
			}
		}
		return dataMap;
	}

	public void setOriginalUnits(String units)
	{
		_originalUnits = units;
	}
}

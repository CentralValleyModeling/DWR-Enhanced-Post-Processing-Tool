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

package gov.ca.water.trendreporting;

import java.time.LocalDate;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.EpptParameter;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputer;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.project.EpptScenarioRun;
import org.json.JSONObject;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-24-2020
 */
class TrendReportDataLoader
{
	private static final Logger LOGGER = Logger.getLogger(TrendReportDataLoader.class.getName());
	private final List<EpptScenarioRun> _scenarioRuns;
	private final List<EpptParameter> _guiLink;
	private final int _start;
	private final int _end;
	private final boolean _taf;
	private final WaterYearDefinition _waterYearDefinition;
	private final List<EpptStatistic> _statistics;
	private final List<MonthPeriod> _monthPeriod;
	private final List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> _waterYearPeriodRangesFilters;
	private final boolean _aggregate;
	private final boolean _difference;

	TrendReportDataLoader(List<EpptScenarioRun> scenarioRuns, List<EpptParameter> guiLink, int start, int end, boolean taf, boolean difference,
						  WaterYearDefinition waterYearDefinition, List<EpptStatistic> statistics, List<MonthPeriod> monthPeriod,
						  List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> waterYearPeriodRangesFilters, boolean aggregate)
	{
		_scenarioRuns = scenarioRuns;
		_guiLink = guiLink;
		_start = start;
		_end = end;
		_taf = taf;
		_difference = difference;
		_waterYearDefinition = waterYearDefinition;
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_waterYearPeriodRangesFilters = waterYearPeriodRangesFilters;
		_aggregate = aggregate;
	}

	public boolean isAggregate()
	{
		return _aggregate;
	}

	List<JSONObject> computeScenarios()
	{
		Optional<String> error = getError();
		if(error.isPresent())
		{
			throw new TrendReportException(error.get());
		}
		else
		{
			return compute();
		}
	}

	private List<JSONObject> compute()
	{
		List<JSONObject> retval = new ArrayList<>();
		WaterYearPeriod waterYearPeriod = new WaterYearPeriod("Entire Range");

		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(_end, waterYearPeriod),
				new WaterYearType(_end, waterYearPeriod));
		YearMonth endYearMonth = waterYearPeriodRange.getEnd(_waterYearDefinition);
		LocalDate end = LocalDate.of(endYearMonth.getYear(), endYearMonth.getMonth(), 1).plusMonths(1).minusDays(2);

		waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(_start, waterYearPeriod),
				new WaterYearType(_start, waterYearPeriod));
		YearMonth startYearMonth = waterYearPeriodRange.getStart(_waterYearDefinition);
		LocalDate start = LocalDate.of(startYearMonth.getYear(), startYearMonth.getMonth(), 1).minusMonths(1).plusDays(2);
		for(EpptParameter parameter : _guiLink)
		{
			GUILinksAllModelsBO guiLink = parameter.getGuiLink();
			Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData;
			if(_difference)
			{
				scenarioRunData = buildDiffSeries(guiLink, start, end);
			}
			else
			{
				scenarioRunData = buildComparisonSeries(guiLink, start, end);
			}
			EpptReportingComputedSet epptReportingComputedSet = EpptReportingComputer.computeForMetrics(scenarioRunData, guiLink.getPlotTitle(), _taf, _waterYearDefinition,
					_monthPeriod, _waterYearPeriodRangesFilters, _statistics);
			JSONObject jsonObject = epptReportingComputedSet.toJson();
			LOGGER.log(Level.FINE, "{0}", jsonObject);
			retval.add(jsonObject);
		}
		return retval;
	}

	private Map<EpptScenarioRun, List<TimeSeriesContainer>> buildComparisonSeries(GUILinksAllModelsBO guiLink, LocalDate start, LocalDate end)
	{
		Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(_scenarioRuns::indexOf));
		for(EpptScenarioRun epptScenarioRun : _scenarioRuns)
		{
			DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(epptScenarioRun, guiLink, _taf, start, end);
			TimeSeriesContainer primarySeries = dssGrabber.getPrimarySeries()[0];
			scenarioRunData.put(epptScenarioRun, Collections.singletonList(primarySeries));
		}
		return scenarioRunData;
	}

	private Map<EpptScenarioRun, List<TimeSeriesContainer>> buildDiffSeries(GUILinksAllModelsBO guiLink, LocalDate start, LocalDate end)
	{
		Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(_scenarioRuns::indexOf));

		Optional<EpptScenarioRun> baseRun = _scenarioRuns.stream().filter(EpptScenarioRun::isBaseSelected).findAny();
		if(baseRun.isPresent())
		{
			DSSGrabber1SvcImpl dssGrabber = buildDssGrabber(baseRun.get(), guiLink, _taf, start, end);
			TimeSeriesContainer baseSeries = dssGrabber.getPrimarySeries()[0];

			for(EpptScenarioRun epptScenarioRun : _scenarioRuns)
			{
				if(!epptScenarioRun.isBaseSelected())
				{
					dssGrabber = buildDssGrabber(epptScenarioRun, guiLink, _taf, start, end);
					TimeSeriesContainer primarySeries = dssGrabber.getPrimarySeries()[0];

					scenarioRunData.put(epptScenarioRun, Collections.singletonList(DSSGrabber1SvcImpl.diffSeries(baseSeries, primarySeries)));
				}
			}
		}
		return scenarioRunData;
	}

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, GUILinksAllModelsBO guiLink, boolean isCFS, LocalDate start, LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = new DSSGrabber1SvcImpl();
		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		dssGrabber.setGuiLink(guiLink);
		dssGrabber.setDateRange(start, end);
		return dssGrabber;
	}


	private Optional<String> getError()
	{
		Optional<String> retval = Optional.empty();
		if(_scenarioRuns.isEmpty())
		{
			retval = Optional.of("No Scenario Runs defined");
		}
		else if(_guiLink.isEmpty() || _guiLink.get(0).getGuiLink() == null)
		{
			retval = Optional.of("No Parameter defined");
		}
		else if(_statistics.isEmpty() || _statistics.get(0).getName().isEmpty())
		{
			retval = Optional.of("No Statistic defined");
		}
		else if(_monthPeriod.isEmpty() || _monthPeriod.get(0).getStart() == null)
		{
			retval = Optional.of("No Seasonal Period defined");
		}
		else if(_waterYearPeriodRangesFilters.isEmpty())
		{
			retval = Optional.of("No Annual Filter defined");
		}
		else if(_waterYearPeriodRangesFilters.stream().anyMatch(filter -> _scenarioRuns.stream().map(filter::get).anyMatch(Objects::isNull)))
		{
			retval = Optional.of("Undefined Annual Period due to invalid water year type definitions");
		}
		else if(_difference && _scenarioRuns.stream().noneMatch(EpptScenarioRun::isBaseSelected))
		{
			retval = Optional.of("No Base scenario selected for Difference calculation");
		}
		else if(_difference && _scenarioRuns.stream().noneMatch(EpptScenarioRun::isAltSelected))
		{
			retval = Optional.of("No Alternative scenario selected for Difference calculation");
		}
		return retval;
	}

	static class TrendReportException extends RuntimeException
	{
		TrendReportException(String error)
		{
			super(error);
		}
	}
}

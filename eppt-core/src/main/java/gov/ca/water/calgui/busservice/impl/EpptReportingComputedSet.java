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
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import org.json.JSONArray;
import org.json.JSONObject;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-26-2019
 */
public final class EpptReportingComputedSet
{
	private static final Logger LOGGER = Logger.getLogger(EpptReportingComputedSet.class.getName());
	private static final String SCENARIO_RUN_DATA = "scenario_run_data";
	private static final String GUI_LINK_TITLE = "gui_link_title";
	private static final String STATISTICS = "statistics";
	private static final String MONTH_PERIOD = "month_period_title";
	private static final String PERIOD_MONTHS = "period_months";
	private static final String COMPARISON_TYPE = "comparison_mode";
	private static final String UNITS = "units";
	private static final String TAF = "taf";
	private final GUILinksAllModelsBO _guiLink;
	private final EpptStatistic _statistics;
	private final MonthPeriod _monthPeriod;
	private final List<EpptReportingComputed> _epptReportingComputed;
	private final PlotConfigurationState.ComparisonType _comparisonType;
	private final boolean _taf;

	private EpptReportingComputedSet(GUILinksAllModelsBO guiLink, EpptStatistic statistics,
									 MonthPeriod monthPeriod, boolean taf,
									 List<EpptReportingComputed> epptReportingComputed,
									 PlotConfigurationState.ComparisonType comparisonType)
	{
		_guiLink = guiLink;
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_taf = taf;
		_epptReportingComputed = epptReportingComputed;
		_comparisonType = comparisonType;
	}


	public static EpptReportingComputedSet computeForMetrics(GUILinksAllModelsBO guiLink, EpptStatistic statistic,
															 MonthPeriod monthPeriod, LocalDate start,
															 LocalDate end, boolean taf, Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
															 PlotConfigurationState.ComparisonType comparisonType,
															 Map<EpptScenarioRun, WaterYearIndex> selectedIndicies,
															 Map<EpptScenarioRun, List<WaterYearIndex>> allIndicies)
	{
		EpptReportingComputer trendReportingComputer = new EpptReportingComputer(guiLink, statistic, monthPeriod,
				selectedIndicies, allIndicies);
		return buildComputedValues(guiLink, statistic, monthPeriod, start, end, taf, scenarioRunData, comparisonType, trendReportingComputer);
	}


	public static EpptReportingComputedSet computeDiffForMetrics(GUILinksAllModelsBO guiLink, EpptStatistic statistic,
																 MonthPeriod monthPeriod, LocalDate start,
																 LocalDate end, boolean taf, EpptScenarioRun baseRun,
																 Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
																 PlotConfigurationState.ComparisonType comparisonType,
																 Map<EpptScenarioRun, WaterYearIndex> selectedIndicies,
																 Map<EpptScenarioRun, List<WaterYearIndex>> allIndicies)
	{
		EpptDiffReportingComputer trendReportingComputer = new EpptDiffReportingComputer(baseRun, guiLink, statistic, monthPeriod,
				selectedIndicies, allIndicies);
		return buildComputedValues(guiLink, statistic, monthPeriod, start, end, taf, scenarioRunData, comparisonType, trendReportingComputer);
	}

	private static EpptReportingComputedSet buildComputedValues(GUILinksAllModelsBO guiLink, EpptStatistic statistic, MonthPeriod monthPeriod,
																LocalDate start, LocalDate end, boolean taf,
																Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
																PlotConfigurationState.ComparisonType comparisonType,
																EpptReportingComputer trendReportingComputer)
	{
		List<EpptReportingComputed> trendReportingComputed = new ArrayList<>();

		for(Map.Entry<EpptScenarioRun, TimeSeriesContainer[]> data : scenarioRunData.entrySet())
		{
			try
			{
				if(taf)
				{
					trendReportingComputed.add(trendReportingComputer.computeTaf(data.getKey(), data.getValue(), start, end));
				}
				else
				{
					trendReportingComputed.add(trendReportingComputer.computeCfs(data.getKey(), data.getValue(), start, end));
				}
			}
			catch(EpptInitializationException e)
			{
				LOGGER.log(Level.SEVERE, "Error calculating Trend Reporting for scenario run: " + data, e);
			}
		}
		return new EpptReportingComputedSet(guiLink, statistic, monthPeriod,
				taf, trendReportingComputed, comparisonType);
	}

	public JSONObject toJson()
	{
		JSONObject jsonObject = new JSONObject();
		JSONArray jsonArray = new JSONArray();
		jsonObject.put(GUI_LINK_TITLE, _guiLink.getPlotTitle());
		jsonObject.put(STATISTICS, _statistics.getName());
		if(!_epptReportingComputed.isEmpty())
		{
			jsonObject.put(UNITS, _epptReportingComputed.get(0).getUnits());
		}
		jsonObject.put(MONTH_PERIOD, _monthPeriod.toString());
		jsonObject.put(SCENARIO_RUN_DATA, jsonArray);
		jsonObject.put(TAF, _taf);
		jsonObject.put(COMPARISON_TYPE, _comparisonType.name());
		JSONArray periodMonths = new JSONArray();
		EpptReportingMonths.getMonths(_monthPeriod).forEach(e -> periodMonths.put(e.getDisplayName(TextStyle.SHORT, Locale.getDefault())));
		jsonObject.put(PERIOD_MONTHS, periodMonths);
		_epptReportingComputed.forEach(e -> jsonArray.put(e.toJson()));
		return jsonObject;
	}
}

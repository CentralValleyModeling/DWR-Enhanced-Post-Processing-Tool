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
import java.util.TreeMap;

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
	private static final String SCENARIO_RUN_DATA = "scenario_run_data";
	private static final String GUI_LINK_TITLE = "gui_link_title";
	private static final String STATISTICS = "statistics";
	private static final String MONTH_PERIOD = "month_period_title";
	private static final String PERIOD_MONTHS = "period_months";
	private static final String COMPARISON_TYPE = "comparison_mode";
	private final EpptStatistic _statistics;
	private final MonthPeriod _monthPeriod;
	private final List<EpptReportingScenarioComputed> _epptReportingComputed;
	private final PlotConfigurationState.ComparisonType _comparisonType;
	private final String _plotTitle;

	private EpptReportingComputedSet(String plotTitle, EpptStatistic statistics,
									 MonthPeriod monthPeriod,
									 List<EpptReportingScenarioComputed> epptReportingComputed,
									 PlotConfigurationState.ComparisonType comparisonType)
	{
		_plotTitle = plotTitle;
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_epptReportingComputed = epptReportingComputed;
		_comparisonType = comparisonType;
	}

	public static EpptReportingComputedSet computeForMetrics(String plotTitle, EpptStatistic statistic,
															 MonthPeriod monthPeriod, boolean taf,
															 Map<EpptScenarioRun, List<String>> primarySuffixes,
															 Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
															 Map<EpptScenarioRun, List<String>> secondarySuffixes,
															 Map<EpptScenarioRun, List<TimeSeriesContainer>> seconaryScenarioRunData,
															 PlotConfigurationState.ComparisonType comparisonType,
															 Map<EpptScenarioRun, WaterYearIndex> selectedIndicies,
															 Map<EpptScenarioRun, List<WaterYearIndex>> allIndicies)
	{
		EpptReportingComputer trendReportingComputer = new EpptReportingComputer(statistic, monthPeriod,
				selectedIndicies, allIndicies);
		return buildComputedValues(plotTitle, statistic, monthPeriod, taf, primarySuffixes, scenarioRunData, secondarySuffixes,
				seconaryScenarioRunData, comparisonType, trendReportingComputer);
	}


	public static EpptReportingComputedSet computeDiffForMetrics(String plotTitle, EpptStatistic statistic,
																 MonthPeriod monthPeriod, boolean taf, EpptScenarioRun baseRun,
																 Map<EpptScenarioRun, List<String>> primarySuffixes,
																 Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
																 Map<EpptScenarioRun, List<String>> secondarySuffixes,
																 Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
																 PlotConfigurationState.ComparisonType comparisonType,
																 Map<EpptScenarioRun, WaterYearIndex> selectedIndicies,
																 Map<EpptScenarioRun, List<WaterYearIndex>> allIndicies)
	{
		EpptDiffReportingComputer trendReportingComputer = new EpptDiffReportingComputer(statistic, monthPeriod,
				selectedIndicies, allIndicies, scenarioRunData.get(baseRun));
		List<TimeSeriesContainer> primaryData = scenarioRunData.remove(baseRun);
		List<TimeSeriesContainer> secondaryData = secondaryScenarioRunData.remove(baseRun);
		EpptReportingComputedSet retval = buildComputedValues(plotTitle, statistic, monthPeriod, taf, primarySuffixes, scenarioRunData,
				secondarySuffixes, secondaryScenarioRunData, comparisonType,
				trendReportingComputer);
		scenarioRunData.put(baseRun, primaryData);
		secondaryScenarioRunData.put(baseRun, secondaryData);
		return retval;
	}

	private static EpptReportingComputedSet buildComputedValues(String plotTitle,
																EpptStatistic statistic,
																MonthPeriod monthPeriod,
																boolean taf,
																Map<EpptScenarioRun, List<String>> primarySuffixes,
																Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
																Map<EpptScenarioRun, List<String>> secondarySuffixes,
																Map<EpptScenarioRun, List<TimeSeriesContainer>> seconaryScenarioRunData,
																PlotConfigurationState.ComparisonType comparisonType,
																EpptReportingComputer trendReportingComputer)
	{
		List<EpptReportingScenarioComputed> trendReportingComputed = new ArrayList<>();

		for(Map.Entry<EpptScenarioRun, List<TimeSeriesContainer>> data : scenarioRunData.entrySet())
		{
			List<TimeSeriesContainer> secondary = seconaryScenarioRunData.getOrDefault(data.getKey(), new ArrayList<>());
			List<String> primaryS = primarySuffixes.getOrDefault(data.getKey(), new ArrayList<>());
			List<String> secondaryS = secondarySuffixes.getOrDefault(data.getKey(), new ArrayList<>());

			if(secondaryS == null)
			{
				secondaryS = new ArrayList<>();
			}
			if(secondary == null)
			{
				secondary = new ArrayList<>();
			}
			if(primaryS == null)
			{
				primaryS = new ArrayList<>();
			}
			if(taf)
			{
				trendReportingComputed.add(
						trendReportingComputer.computeTaf(data.getKey(), primaryS, data.getValue(), secondaryS, secondary));
			}
			else
			{
				trendReportingComputed.add(
						trendReportingComputer.computeCfs(data.getKey(), primaryS, data.getValue(), secondaryS, secondary));
			}
		}
		return new EpptReportingComputedSet(plotTitle, statistic, monthPeriod,
				trendReportingComputed, comparisonType);
	}

	public List<EpptReportingScenarioComputed> getEpptReportingComputed()
	{
		return _epptReportingComputed;
	}

	public JSONObject toJson()
	{
		JSONObject jsonObject = new JSONObject();
		JSONArray jsonArray = new JSONArray();
		jsonObject.put(GUI_LINK_TITLE, _plotTitle);
		jsonObject.put(STATISTICS, _statistics.getName());
		jsonObject.put(MONTH_PERIOD, _monthPeriod.toString());
		jsonObject.put(SCENARIO_RUN_DATA, jsonArray);
		jsonObject.put(COMPARISON_TYPE, _comparisonType.name());
		JSONArray periodMonths = new JSONArray();
		EpptReportingMonths.getMonths(_monthPeriod).forEach(e -> periodMonths.put(e.getDisplayName(TextStyle.SHORT, Locale.getDefault())));
		jsonObject.put(PERIOD_MONTHS, periodMonths);
		_epptReportingComputed.stream()
							  .map(EpptReportingScenarioComputed::toJson)
							  .forEach(jsonArray::put);
		return jsonObject;
	}
}

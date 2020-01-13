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

import java.time.LocalDateTime;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableMap;
import java.util.SortedMap;
import java.util.TreeMap;

import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-26-2019
 */
public class EpptReportingComputed
{
	private static final String SCENARIO_NAME_KEY = "scenario_name";
	private static final String FULL_TIME_SERIES = "full_time_series";
	private static final String PERIOD_FILTERED_TIME_SERIES = "period_filtered_time_series";
	private static final String STATISTICALLY_COMPUTED_YEARLY = "statistically_computed_time_series_yearly";
	private static final String STATISTICALLY_COMPUTED_MONTHLY = "statistically_computed_time_series_monthly";
	private static final String STATISTICALLY_COMPUTER_TIME_SERIES_WYT = "statistically_computed_time_series_wyt";
	private static final String WATER_YEAR_PERIOD = "water_year_period";
	private static final String WATER_YEAR_PERIOD_VALUES = "water_year_period_values";
	private static final String SCENARIO_COLOR = "scenario_color";
	private final EpptScenarioRun _epptScenarioRun;
	private final NavigableMap<LocalDateTime, Double> _fullTimeSeries;
	private final NavigableMap<Integer, Double> _annualPeriodFilteredTimeSeries;
	private final NavigableMap<Month, Double> _statisticallyComputedMonthly;
	private final String _units;
	private final SortedMap<WaterYearPeriod, Double> _statisticallyComputedTimeSeriesWyt;
	private final Double _yearlyStatistic;

	EpptReportingComputed(EpptScenarioRun epptScenarioRun, NavigableMap<LocalDateTime, Double> fullTimeSeries,
						  NavigableMap<Integer, Double> annualPeriodFiltered, Double yearlyStatistic,
						  SortedMap<Month, Double> statisticallyComputedMonthly,
						  SortedMap<WaterYearPeriod, Double> statisticallyComputedTimeSeriesWyt, String units)
	{
		_epptScenarioRun = epptScenarioRun;
		_fullTimeSeries = fullTimeSeries;
		_annualPeriodFilteredTimeSeries = annualPeriodFiltered;
		_yearlyStatistic = yearlyStatistic;
		_statisticallyComputedMonthly = new TreeMap<>(statisticallyComputedMonthly);
		_statisticallyComputedTimeSeriesWyt = statisticallyComputedTimeSeriesWyt;
		_units = units;
	}

	JSONObject toJson()
	{
		JSONObject jsonObject = new JSONObject();
		jsonObject.put(SCENARIO_NAME_KEY, _epptScenarioRun.getName());
		jsonObject.put(FULL_TIME_SERIES, buildTimeSeriesMap(_fullTimeSeries));
		jsonObject.put(PERIOD_FILTERED_TIME_SERIES, buildYearMap(_annualPeriodFilteredTimeSeries));
		jsonObject.put(STATISTICALLY_COMPUTED_YEARLY, _yearlyStatistic);
		jsonObject.put(STATISTICALLY_COMPUTED_MONTHLY, buildMonthMap(_statisticallyComputedMonthly));
		jsonObject.put(STATISTICALLY_COMPUTER_TIME_SERIES_WYT, buildWytMap(_statisticallyComputedTimeSeriesWyt));
		jsonObject.put(SCENARIO_COLOR, Constant.colorToHex(_epptScenarioRun.getColor()));
		return jsonObject;
	}

	private JSONArray buildWytMap(SortedMap<WaterYearPeriod, Double> statisticallyComputedTimeSeriesWyt)
	{
		JSONArray jsonArray = new JSONArray();
		statisticallyComputedTimeSeriesWyt.entrySet()
										  .stream()
										  .map(this::extractWytArray)
										  .forEach(jsonArray::put);
		return jsonArray;
	}

	private JSONObject extractWytArray(Map.Entry<WaterYearPeriod, Double> waterYearPeriodSortedMapEntry)
	{
		JSONObject retval = new JSONObject();
		retval.put(WATER_YEAR_PERIOD, waterYearPeriodSortedMapEntry.getKey().getPeriodName());
		retval.put(WATER_YEAR_PERIOD_VALUES, waterYearPeriodSortedMapEntry.getValue());
		return retval;
	}

	String getUnits()
	{
		return _units;
	}

	private JSONArray buildTimeSeriesMap(Map<LocalDateTime, Double> fullTimeSeries)
	{
		JSONArray jsonArray = new JSONArray();
		fullTimeSeries.entrySet()
					  .stream().map(this::extractDateArray)
					  .forEach(jsonArray::put);
		return jsonArray;
	}

	private JSONArray extractDateArray(Map.Entry<LocalDateTime, Double> e)
	{
		JSONArray retval = new JSONArray();
		long l = ZonedDateTime.of(e.getKey(), ZoneId.systemDefault()).toInstant().toEpochMilli();
		retval.put(l);
		retval.put(e.getValue());
		return retval;
	}

	private JSONArray extractMonthArray(Map.Entry<Month, Double> e)
	{
		JSONArray retval = new JSONArray();
		retval.put(e.getKey().getDisplayName(TextStyle.SHORT, Locale.getDefault()));
		retval.put(e.getValue());
		return retval;
	}

	private JSONArray extractYearArray(Map.Entry<Integer, Double> e)
	{
		JSONArray retval = new JSONArray();
		retval.put(e.getKey());
		retval.put(e.getValue());
		return retval;
	}

	private JSONArray buildYearMap(Map<Integer, Double> fullTimeSeries)
	{
		JSONArray jsonArray = new JSONArray();
		fullTimeSeries.entrySet()
					  .stream().map(this::extractYearArray)
					  .forEach(jsonArray::put);
		return jsonArray;
	}

	private JSONArray buildMonthMap(Map<Month, Double> fullTimeSeries)
	{
		JSONArray jsonArray = new JSONArray();
		fullTimeSeries.entrySet()
					  .stream().map(this::extractMonthArray)
					  .forEach(jsonArray::put);
		return jsonArray;
	}

}

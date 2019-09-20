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

package gov.ca.water.calgui.compute;

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

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import org.json.JSONArray;
import org.json.JSONObject;

import static java.util.stream.Collectors.toMap;

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
	private static final String STATISTICALLY_COMPUTED_TIME_SERIES = "statistically_computed_time_series";
	private static final String SCENARIO_COLOR = "scenario_color";
	private final EpptScenarioRun _epptScenarioRun;
	private final NavigableMap<LocalDateTime, Double> _fullTimeSeries;
	private final NavigableMap<LocalDateTime, Double> _periodFilteredTimeSeries;
	private final NavigableMap<Month, Double> _statisticallyComputedTimeSeries;
	private final String _units;

	public EpptReportingComputed(EpptScenarioRun epptScenarioRun, Map<LocalDateTime, Double> fullTimeSeries,
								 Map<LocalDateTime, Double> periodFilteredTimeSeries, SortedMap<Month, Double> statisticallyComputedTimeSeries,
								 String units)
	{
		_epptScenarioRun = epptScenarioRun;
		_fullTimeSeries = new TreeMap<>(fullTimeSeries);
		_periodFilteredTimeSeries = new TreeMap<>(periodFilteredTimeSeries);
		_statisticallyComputedTimeSeries = new TreeMap<>(statisticallyComputedTimeSeries);
		_units = units;
	}

	public JSONObject toJson()
	{
		JSONObject jsonObject = new JSONObject();
		jsonObject.put(SCENARIO_NAME_KEY, _epptScenarioRun.getName());
		jsonObject.put(FULL_TIME_SERIES, buildTimeSeriesMap(_fullTimeSeries));
		jsonObject.put(PERIOD_FILTERED_TIME_SERIES, buildTimeSeriesMap(_periodFilteredTimeSeries));
		jsonObject.put(STATISTICALLY_COMPUTED_TIME_SERIES, buildMonthMap(_statisticallyComputedTimeSeries));
		jsonObject.put(SCENARIO_COLOR, Constant.colorToHex(_epptScenarioRun.getColor()));
		return jsonObject;
	}

	public String getUnits()
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

	private JSONArray buildMonthMap(Map<Month, Double> fullTimeSeries)
	{
		JSONArray jsonArray = new JSONArray();
		fullTimeSeries.entrySet()
					  .stream().map(this::extractMonthArray)
					  .forEach(jsonArray::put);
		return jsonArray;
	}

}

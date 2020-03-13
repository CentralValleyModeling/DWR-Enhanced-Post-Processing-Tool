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
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;

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
public final class EpptReportingComputedSet
{
	private static final String SCENARIO_RUN_DATA = "scenario_run_data";
	private static final String GUI_LINK_TITLE = "gui_link_title";
	private static final String COMPARISON_TYPE = "comparison_mode";
	private static final String SCENARIO_NAME_KEY = "scenario_name";
	private static final String MONTHLY_FILTERS = "monthly_filters";
	private static final String TS_LIST = "ts_list";
	private static final String ANNUAL_FILTERS = "annual_filters";
	private static final String TS_NAME = "ts_name";
	private static final String DATA = "data";
	private static final String SCENARIO_COLOR = "scenario_color";
	private static final String UNITS = "units";
	private static final String FIRST_RECORD = "first_record";
	private static final String LAST_RECORD = "last_record";
	private static final String IS_INSTANTANEOUS = "is_instantaneous";
	private final List<EpptReportingScenarioComputed> _epptReportingComputed;
	private final String _units;
	private final LocalDateTime _firstRecord;
	private final LocalDateTime _lastRecord;
	private final boolean _isInstantaneous;
	private final String _plotTitle;

	EpptReportingComputedSet(String plotTitle, List<EpptReportingScenarioComputed> epptReportingComputed, String units, LocalDateTime firstRecord,
							 LocalDateTime lastRecord, boolean isInstantaneous)
	{
		_plotTitle = plotTitle;
		_epptReportingComputed = epptReportingComputed;
		_units = units;
		_firstRecord = firstRecord;
		_lastRecord = lastRecord;
		_isInstantaneous = isInstantaneous;
	}

	public String getUnits()
	{
		return _units;
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
		jsonObject.put(SCENARIO_RUN_DATA, jsonArray);
		jsonObject.put(UNITS, _units);
		long firstRecord = ZonedDateTime.of(_firstRecord, ZoneId.systemDefault()).toInstant().toEpochMilli();
		jsonObject.put(FIRST_RECORD, firstRecord);
		long lastRecord = ZonedDateTime.of(_lastRecord, ZoneId.systemDefault()).toInstant().toEpochMilli();
		jsonObject.put(LAST_RECORD, lastRecord);
		jsonObject.put(IS_INSTANTANEOUS, _isInstantaneous);
		_epptReportingComputed.stream()
							  .map(EpptReportingScenarioComputed::toJson)
							  .forEach(jsonArray::put);
		return jsonObject;
	}

	public static class EpptReportingMonthComputed
	{
		private final List<EpptReportingComputed> _epptReportingComputed;

		EpptReportingMonthComputed(List<EpptReportingComputed> epptReportingComputed)
		{
			_epptReportingComputed = epptReportingComputed;
		}

		public List<EpptReportingComputed> getEpptReportingComputed()
		{
			return _epptReportingComputed;
		}

		private JSONObject toJson()
		{
			JSONObject retval = new JSONObject();
			JSONArray jsonArray = new JSONArray();
			_epptReportingComputed.stream().map(EpptReportingComputed::toJson)
								  .forEach(jsonArray::put);
			retval.put(ANNUAL_FILTERS, jsonArray);
			return retval;
		}
	}

	public static class EpptReportingTs
	{
		private final String _tsName;
		private final List<EpptReportingMonthComputed> _monthComputed;

		EpptReportingTs(String tsName, List<EpptReportingMonthComputed> monthComputed)
		{
			_tsName = tsName;
			_monthComputed = monthComputed;
		}

		public String getTsName()
		{
			return _tsName;
		}

		public List<EpptReportingMonthComputed> getMonthComputed()
		{
			return _monthComputed;
		}

		private JSONObject toJson()
		{
			JSONObject retval = new JSONObject();
			JSONArray jsonArray = new JSONArray();
			_monthComputed.stream().map(EpptReportingMonthComputed::toJson)
						  .forEach(jsonArray::put);
			retval.put(MONTHLY_FILTERS, jsonArray);
			retval.put(TS_NAME, _tsName);
			return retval;
		}
	}

	public static class EpptReportingScenarioComputed
	{
		private final EpptScenarioRun _epptScenarioRun;
		private final List<EpptReportingTs> _tsComputed;

		EpptReportingScenarioComputed(EpptScenarioRun epptScenarioRun, List<EpptReportingTs> tsComputed)
		{
			_epptScenarioRun = epptScenarioRun;
			_tsComputed = tsComputed;
		}

		public EpptScenarioRun getEpptScenarioRun()
		{
			return _epptScenarioRun;
		}

		public List<EpptReportingTs> getTsComputed()
		{
			return _tsComputed;
		}

		private JSONObject toJson()
		{
			JSONObject retval = new JSONObject();
			retval.put(SCENARIO_NAME_KEY, _epptScenarioRun.getName());
			JSONArray jsonArray = new JSONArray();
			_tsComputed.stream().map(EpptReportingTs::toJson)
					   .forEach(jsonArray::put);
			retval.put(TS_LIST, jsonArray);
			retval.put(SCENARIO_COLOR, Constant.colorToHex(_epptScenarioRun.getColor()));
			return retval;
		}
	}
}

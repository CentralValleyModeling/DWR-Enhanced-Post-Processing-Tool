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
import java.util.Map;
import java.util.Objects;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import org.json.JSONArray;
import org.json.JSONObject;

import hec.heclib.dss.DSSPathname;
import hec.io.TimeSeriesContainer;

import static java.util.stream.Collectors.toList;

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
	private static final String SCENARIO_NAME_KEY = "scenario_name";
	private static final String MONTHLY_FILTERS = "monthly_filters";
	private static final String TS_LIST = "ts_list";
	private static final String ANNUAL_FILTERS = "annual_filters";
	private static final String TS_NAME = "ts_name";
	private static final String SCENARIO_COLOR = "scenario_color";
	private static final String SCENARIO_MODEL = "scenario_model";
	private static final String TIME_SERIES_METADATA = "time_series_metadata";
	private static final String TIME_SERIES_DSS_PATH = "dss_path";
	private static final String TIME_SERIES_FILE_ALIAS = "time_series_file_alias";
	private static final String UNITS = "units";
	private static final String TS_DESCRIPTOR = "ts_descriptor";
	private static final String FIRST_RECORD = "first_record";
	private static final String LAST_RECORD = "last_record";
	private static final String IS_INSTANTANEOUS = "is_instantaneous";
	private final List<EpptReportingScenarioComputed> _epptReportingComputed;
	private final String _units;
	private final LocalDateTime _firstRecord;
	private final LocalDateTime _lastRecord;
	private final boolean _isInstantaneous;
	private final Map<EpptScenarioRun, List<TimeSeriesContainer>> _scenarioRunData;
	private final String _plotTitle;

	EpptReportingComputedSet(String plotTitle, List<EpptReportingScenarioComputed> epptReportingComputed, String units, LocalDateTime firstRecord, LocalDateTime lastRecord, boolean isInstantaneous,
							 Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData)
	{
		_plotTitle = plotTitle;
		_epptReportingComputed = epptReportingComputed;
		_units = units;
		_firstRecord = firstRecord;
		_lastRecord = lastRecord;
		_isInstantaneous = isInstantaneous;
		_scenarioRunData = scenarioRunData;
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
		jsonObject.put(TS_DESCRIPTOR, buildTimeSeriesDescriptor());
		if(_firstRecord != null)
		{
			long firstRecord = ZonedDateTime.of(_firstRecord, ZoneId.systemDefault()).toInstant().toEpochMilli();
			jsonObject.put(FIRST_RECORD, firstRecord);
		}
		if(_lastRecord != null)
		{
			long lastRecord = ZonedDateTime.of(_lastRecord, ZoneId.systemDefault()).toInstant().toEpochMilli();
			jsonObject.put(LAST_RECORD, lastRecord);
		}
		jsonObject.put(IS_INSTANTANEOUS, _isInstantaneous);
		_epptReportingComputed.stream()
							  .map(EpptReportingScenarioComputed::toJson)
							  .forEach(jsonArray::put);
		return jsonObject;
	}

	private JSONArray buildTimeSeriesDescriptor()
	{
		JSONArray retval = new JSONArray();
		for(Map.Entry<EpptScenarioRun, List<TimeSeriesContainer>> entry : _scenarioRunData.entrySet())
		{
			EpptScenarioRun key = entry.getKey();
			JSONObject scenarioRow = new JSONObject();
			scenarioRow.put(SCENARIO_NAME_KEY, key.getName());
			scenarioRow.put(SCENARIO_COLOR, Constant.colorToHex(key.getColor()));
			scenarioRow.put(SCENARIO_MODEL, key.getModel().toString());
			List<TimeSeriesContainer> value = entry.getValue().stream().filter(Objects::nonNull).collect(toList());
			scenarioRow.put(TIME_SERIES_METADATA, buildTimeSeriesMetaData(value));
			retval.put(scenarioRow);
		}
		return retval;
	}

	private JSONArray buildTimeSeriesMetaData(List<TimeSeriesContainer> tsContainers)
	{
		JSONArray retval = new JSONArray();
		tsContainers.stream().map(tsc->
		{
			JSONObject jsonObject = new JSONObject();
			jsonObject.put(TIME_SERIES_FILE_ALIAS,tsc.fileName);
			DSSPathname dssPathname = new DSSPathname();
			dssPathname.setAPart(tsc.watershed);
			dssPathname.setBPart(tsc.getLocationName());
			dssPathname.setCPart(tsc.getParameterName());
			dssPathname.setFPart(tsc.getVersionName());
			jsonObject.put(TIME_SERIES_DSS_PATH,dssPathname.getPathname());
			return jsonObject;
		}).forEach(retval::put);
		return retval;
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
		private final String _supplementalInfo;
		private final List<EpptReportingMonthComputed> _monthComputed;

		EpptReportingTs(String tsName, String supplementalInfo, List<EpptReportingMonthComputed> monthComputed)
		{
			_tsName = tsName;
			_supplementalInfo = supplementalInfo;
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

		public String getSupplementalInfo()
		{
			return _supplementalInfo;
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

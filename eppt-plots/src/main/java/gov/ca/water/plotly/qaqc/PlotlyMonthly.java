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

package gov.ca.water.plotly.qaqc;

import java.nio.file.Path;
import java.time.Month;
import java.time.format.TextStyle;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.PlotlyChart;
import javafx.scene.paint.Color;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-19-2019
 */
public class PlotlyMonthly extends PlotlyChart
{
	private static final Path TEMPLATE = Constant.QA_QC_TEMPLATE_PATH.resolve("monthly_chart.json");

	private final String _title;
	private final String _xAxis;
	private final String _yAxis;
	private final Map<EpptScenarioRun, List<MonthlyData>> _monthlyData;
	private final Map<EpptScenarioRun, Map<String, List<MonthlyData>>> _thresholdData;

	public PlotlyMonthly(String title, String xAxis, String yAxis, Map<EpptScenarioRun, List<MonthlyData>> primaryData,
						 Map<EpptScenarioRun, Map<String, List<MonthlyData>>> thresholdData)
	{
		_title = title;
		_xAxis = xAxis;
		_yAxis = yAxis;
		_monthlyData = primaryData;
		_thresholdData = thresholdData;
	}

	@Override
	protected Path getTemplatePath()
	{
		return TEMPLATE;
	}

	@Override
	protected JSONObject buildLayout(JSONObject template)
	{
		template.put("title", _title);
		JSONObject xAxis = template.getJSONObject("xaxis");
		JSONObject yAxis = template.getJSONObject("yaxis");
		JSONObject xAxisTitle = xAxis.getJSONObject("title");
		xAxisTitle.put("text", _xAxis);
		JSONObject yAxisTitle = yAxis.getJSONObject("title");
		yAxisTitle.put("text", _yAxis);
		template.put("xaxis", xAxis);
		template.put("yaxis", yAxis);
		return template;
	}

	@Override
	protected JSONArray buildDataArray(JSONArray template)
	{
		JSONArray dataArray = new JSONArray();
		JSONObject templateTrace = template.getJSONObject(0);
		for(Map.Entry<EpptScenarioRun, List<MonthlyData>> entry : _monthlyData.entrySet())
		{
			EpptScenarioRun key = entry.getKey();
			List<MonthlyData> value = entry.getValue();
			JSONObject primaryTrace = buildPrimaryTrace(templateTrace, key, value);
			dataArray.put(primaryTrace);
			Optional<Map<String, List<MonthlyData>>> thresholdsOpt = _thresholdData.keySet().stream()
																			.filter(k -> Objects.equals(k.getName(), key.getName())).findAny()
																			.map(_thresholdData::get);

			if(thresholdsOpt.isPresent())
			{
				int index = 0;
				for(Map.Entry<String, List<MonthlyData>> thresholdData : thresholdsOpt.get().entrySet())
				{
					dataArray.put(buildThresholdTrace(templateTrace, key, thresholdData.getKey(), thresholdData.getValue(), index));
					index++;
				}
			}
		}
		return dataArray;
	}

	private JSONObject buildThresholdTrace(JSONObject template, EpptScenarioRun epptScenarioRun, String thresholdName, List<MonthlyData> data, int index)
	{
		JSONObject retval = new JSONObject(template.toString());
		JSONArray xArray = new JSONArray();
		JSONArray yArray = new JSONArray();
		for(MonthlyData entry : data)
		{
			xArray.put(entry._month.getDisplayName(TextStyle.SHORT, Locale.getDefault()));
			yArray.put(entry._data);
		}
		JSONObject marker = buildMarker(template.getJSONObject("marker"), epptScenarioRun);
		retval.put("name", thresholdName);
		retval.put("x", xArray);
		retval.put("y", yArray);
		retval.put("marker", marker);
		JSONObject line = new JSONObject();
		line.put("width", 1);
		line.put("dash", Constant.getPlotlyThresholdLineDash(index));
		retval.put("line", line);
		return retval;
	}


	private JSONObject buildPrimaryTrace(JSONObject template, EpptScenarioRun scenarioRun,
										 List<MonthlyData> monthlyData)
	{
		JSONObject retval = new JSONObject(template.toString());
		JSONArray xArray = new JSONArray();
		JSONArray yArray = new JSONArray();
		for(MonthlyData entry : monthlyData)
		{
			xArray.put(entry._month.getDisplayName(TextStyle.SHORT, Locale.getDefault()));
			yArray.put(entry._data);
		}
		JSONObject marker = buildMarker(template.getJSONObject("marker"), scenarioRun);
		retval.put("name", scenarioRun.getName());
		retval.put("x", xArray);
		retval.put("y", yArray);
		retval.put("marker", marker);
		return retval;
	}

	private JSONObject buildMarker(JSONObject templateMarker, EpptScenarioRun scenarioRun)
	{
		JSONObject marker = new JSONObject(templateMarker.toString());
		Color color = scenarioRun.getColor();
		marker.put("color", Constant.colorToHex(color));
		return marker;
	}

	public static class MonthlyData
	{
		private final Month _month;
		private final Double _data;

		public MonthlyData(Month month, Double data)
		{
			_month = month;
			_data = data;
		}
	}

	@Override
	public String getPlotType()
	{
		return "Monthly Plot";
	}
}

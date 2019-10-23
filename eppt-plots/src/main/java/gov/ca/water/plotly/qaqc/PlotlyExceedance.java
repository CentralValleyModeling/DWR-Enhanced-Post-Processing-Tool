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
import java.util.Map;
import java.util.NavigableMap;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.ExceedanceData;
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
public class PlotlyExceedance extends PlotlyChart
{
	private static final Path TEMPLATE = Constant.QA_QC_TEMPLATE_PATH.resolve("exceedance_chart.json");

	private final String _title;
	private final String _xAxis;
	private final String _yAxis;
	private final EpptScenarioRun _base;
	private final ExceedanceData _baseData;
	private final Map<EpptScenarioRun, ExceedanceData> _alternativeData;

	public PlotlyExceedance(String title, String xAxis, String yAxis, EpptScenarioRun base, ExceedanceData baseData,
							Map<EpptScenarioRun, ExceedanceData> alternativeData)
	{
		_title = title;
		_xAxis = xAxis;
		_yAxis = yAxis;
		_base = base;
		_baseData = baseData;
		_alternativeData = alternativeData;
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
		JSONObject baseTrace = buildPrimaryTrace(templateTrace, _base, _baseData);
		dataArray.put(baseTrace);
		int index = 0;
		for(Map.Entry<String, NavigableMap<Double, Double>> thresholdData : _baseData.getThresholdData().entrySet())
		{
			dataArray.put(buildThresholdTrace(templateTrace, thresholdData.getKey(), _base, thresholdData.getValue(), index));
			index++;
		}
		for(Map.Entry<EpptScenarioRun, ExceedanceData> entry : _alternativeData.entrySet())
		{
			EpptScenarioRun key = entry.getKey();
			ExceedanceData value = entry.getValue();
			JSONObject altTrace = buildPrimaryTrace(templateTrace, key, value);
			index = 0;
			for(Map.Entry<String, NavigableMap<Double, Double>> thresholdData : value.getThresholdData().entrySet())
			{
				dataArray.put(buildThresholdTrace(templateTrace, thresholdData.getKey(), key, thresholdData.getValue(), index));
				index++;
			}
			dataArray.put(altTrace);
		}
		return dataArray;
	}

	private JSONObject buildThresholdTrace(JSONObject template, String scenarioName, EpptScenarioRun scenarioRun, NavigableMap<Double, Double> data, int index)
	{
		JSONObject retval = new JSONObject(template.toString());
		JSONArray xArray = new JSONArray();
		JSONArray yArray = new JSONArray();
		for(NavigableMap.Entry<Double, Double> entry : data.entrySet())
		{
			xArray.put(entry.getKey());
			yArray.put(entry.getValue());
		}
		JSONObject marker = buildMarker(template.getJSONObject("marker"), scenarioRun);
		retval.put("name", scenarioName);
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
										 ExceedanceData exceedanceData)
	{
		JSONObject retval = new JSONObject(template.toString());
		JSONArray xArray = new JSONArray();
		JSONArray yArray = new JSONArray();
		for(NavigableMap.Entry<Double, Double> entry : exceedanceData.getPrimaryData().entrySet())
		{
			xArray.put(entry.getKey());
			yArray.put(entry.getValue());
		}
		JSONObject marker = buildMarker(template.getJSONObject("marker"), scenarioRun);
		retval.put("name", exceedanceData.getScenarioName());
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

	@Override
	public String getPlotType()
	{
		return "Exceedance Plot";
	}
}

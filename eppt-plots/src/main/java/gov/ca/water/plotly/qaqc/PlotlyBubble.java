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
import java.util.List;
import java.util.Map;

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
public class PlotlyBubble extends PlotlyChart
{
	private static final Path TEMPLATE = Constant.QA_QC_TEMPLATE_PATH.resolve("bubble_chart.json");

	private final String _title;
	private final String _xAxis;
	private final String _yAxis;
	private final EpptScenarioRun _base;
	private final BubbleData _baseData;
	private final Map<EpptScenarioRun, BubbleData> _alternativeData;

	public PlotlyBubble(String title, String xAxis, String yAxis, EpptScenarioRun base, BubbleData baseData,
						Map<EpptScenarioRun, BubbleData> alternativeData)
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
		int alternativeCount = _alternativeData.size();
		JSONObject grid = template.getJSONObject("grid");
		grid.put("rows", 1);
		grid.put("columns", alternativeCount + 1);
		template.put("title", _title);
		template.put("grid", grid);
		JSONObject xAxis = template.getJSONObject("xaxis");
		JSONObject yAxis = template.getJSONObject("yaxis");
		JSONObject xAxisTitle = xAxis.getJSONObject("title");
		xAxisTitle.put("text", _xAxis);
		JSONObject yAxisTitle = yAxis.getJSONObject("title");
		yAxisTitle.put("text", _yAxis);

		if(template.has("annotations"))
		{
			JSONArray annotations = template.getJSONArray("annotations");
			if(annotations.length() > 0)
			{
				JSONObject annotation = annotations.getJSONObject(0);
				annotation.put("x", _baseData._xData.get(_baseData._xData.size() / 2));
				annotation.put("y", _baseData._yData.get(_baseData._yData.size() / 2));
				annotation.put("xref", "x");
				annotation.put("yref", "y");
				annotation.put("text", "Year");
			}
		}
		int i = 1;
		for(Map.Entry<EpptScenarioRun, BubbleData> entry : _alternativeData.entrySet())
		{
			String xaxisRef = "xaxis" + (i + 1);
			template.put(xaxisRef, xAxis);

			if(template.has("annotations"))
			{
				JSONArray annotations = template.getJSONArray("annotations");
				if(annotations.length() > 0)
				{
					JSONObject annotation = new JSONObject(annotations.getJSONObject(0).toString());
					BubbleData value = entry.getValue();
					annotation.put("x", value._xData.get(value._xData.size() / 2));
					annotation.put("y", value._yData.get(value._yData.size() / 2));
					annotation.put("xref", "x" + (i + 1));
					annotation.put("text", "Year");
					annotations.put(annotation);
				}
			}
			i++;
		}

		return template;
	}

	@Override
	protected JSONArray buildDataArray(JSONArray template)
	{
		JSONArray dataArray = new JSONArray();
		JSONObject templateTrace = template.getJSONObject(0);
		JSONObject baseTrace = buildTrace(templateTrace, "Base", _base, 0, _baseData);
		dataArray.put(baseTrace);
		int i = 0;
		for(Map.Entry<EpptScenarioRun, BubbleData> entry : _alternativeData.entrySet())
		{
			EpptScenarioRun key = entry.getKey();
			BubbleData value = entry.getValue();
			JSONObject altTrace = buildTrace(templateTrace, "Alt", key, i + 1, value);
			dataArray.put(altTrace);
			i++;
		}
		return dataArray;
	}


	private JSONObject buildTrace(JSONObject template, String scenarioName, EpptScenarioRun scenarioRun, int count, BubbleData bubbleData)
	{
		JSONObject retval = new JSONObject(template.toString());
		JSONArray xArray = new JSONArray(bubbleData._xData);
		JSONArray yArray = new JSONArray(bubbleData._yData);
		JSONArray markerArray = new JSONArray(bubbleData._markers);
		JSONObject marker = buildMarker(template.getJSONObject("marker"), scenarioRun);
		retval.put("name", scenarioName);
		retval.put("x", xArray);
		retval.put("y", yArray);
		retval.put("text", markerArray);
		retval.put("marker", marker);
		String xaxis = (count == 0) ? "x" : ("x" + (count + 1));
		retval.put("xaxis", xaxis);
		return retval;
	}

	private JSONObject buildMarker(JSONObject templateMarker, EpptScenarioRun scenarioRun)
	{
		JSONObject marker = new JSONObject(templateMarker.toString());
		JSONObject line = marker.getJSONObject("line");
		Color color = scenarioRun.getColor();
		line.put("color", Constant.colorToHex(color));
		line.put("width", "2");
		marker.put("line", line);
		return marker;
	}

	public static class BubbleData
	{
		private final List<Double> _xData;
		private final List<Double> _yData;
		private final List<String> _markers;

		public BubbleData(List<Double> xData, List<Double> yData, List<String> markers)
		{
			_xData = xData;
			_yData = yData;
			_markers = markers;
		}
	}
}

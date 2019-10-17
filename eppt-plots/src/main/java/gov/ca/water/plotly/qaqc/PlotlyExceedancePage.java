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
import java.util.Collection;
import java.util.DoubleSummaryStatistics;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Objects;

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
public class PlotlyExceedancePage extends PlotlyChart
{
	private static final Path TEMPLATE = Constant.QA_QC_TEMPLATE_PATH.resolve("exceedance_page_chart.json");

	private final String _title;
	private final String _yAxis;
	private final Map<EpptScenarioRun, ExceedanceMonthData> _exceedanceData;

	public PlotlyExceedancePage(String title, String yAxis, Map<EpptScenarioRun, ExceedanceMonthData> exceedanceData)
	{
		_title = title;
		_yAxis = yAxis;
		_exceedanceData = exceedanceData;
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
		yAxis.put("range", buildRange());
		xAxisTitle.put("text", Month.JANUARY.getDisplayName(TextStyle.FULL, Locale.getDefault()));
		JSONObject yAxisTitle = yAxis.getJSONObject("title");
//		yAxisTitle.put("text", _yAxis);
		template.put("xaxis", xAxis);
		template.put("yaxis", yAxis);
		for(int i = 2; i <= 12; i++)
		{
			xAxis = new JSONObject(xAxis.toString());
			yAxis = new JSONObject(yAxis.toString());
			yAxisTitle = yAxis.getJSONObject("title");
			xAxisTitle = xAxis.getJSONObject("title");
			xAxisTitle.put("text", Month.values()[i - 1].getDisplayName(TextStyle.FULL, Locale.getDefault()));
			template.put("xaxis" + i, xAxis);
			template.put("yaxis" + i, yAxis);
			if(i == 7)
			{
				yAxisTitle.put("text", _yAxis);
			}
			else
			{
				yAxisTitle.remove("text");
			}
		}
		return template;
	}

	private JSONArray buildRange()
	{
		DoubleSummaryStatistics summaryStatistics = _exceedanceData.values()
																   .stream()
																   .map(ExceedanceMonthData::getData)
																   .map(Map::values)
																   .flatMap(Collection::stream)
																   .map(ExceedanceData::getPrimaryData)
																   .map(Map::values)
																   .flatMap(Collection::stream)
																   .filter(Objects::nonNull)
																   .mapToDouble(v -> v)
																   .summaryStatistics();
		double min = summaryStatistics.getMin();
		double max = summaryStatistics.getMax();
		if(min == Double.POSITIVE_INFINITY || min == Double.NEGATIVE_INFINITY)
		{
			min = 0;
		}
		if(max == Double.POSITIVE_INFINITY || max == Double.NEGATIVE_INFINITY)
		{
			max = 100;
		}

		return new JSONArray(new double[]{min - 10, max + 10});
	}

	@Override
	protected JSONArray buildDataArray(JSONArray template)
	{
		JSONArray dataArray = new JSONArray();
		JSONObject templateTrace = template.getJSONObject(0);

		for(Map.Entry<EpptScenarioRun, ExceedanceMonthData> entry : _exceedanceData.entrySet())
		{
			addMonthData(dataArray, templateTrace, entry.getKey(), entry.getValue());
		}
		return dataArray;
	}

	private void addMonthData(JSONArray dataArray, JSONObject templateTrace, EpptScenarioRun scenarioRun,
							  ExceedanceMonthData value)
	{
		Map<Month, ExceedanceData> monthlyData = value._data;
		int gridIndex = 1;
		for(Map.Entry<Month, ExceedanceData> entry : monthlyData.entrySet())
		{
			ExceedanceData exceedanceData = entry.getValue();
			JSONObject baseTrace = buildPrimaryTrace(templateTrace, scenarioRun.getName(), scenarioRun, exceedanceData, gridIndex);
			dataArray.put(baseTrace);
			int index = 0;
			for(Map.Entry<String, NavigableMap<Double, Double>> thresholdData : exceedanceData.getThresholdData().entrySet())
			{
				dataArray.put(buildThresholdTrace(templateTrace, thresholdData.getKey(), scenarioRun, thresholdData.getValue(), index, gridIndex));
				index++;
			}
			gridIndex++;
		}
	}

	private JSONObject buildThresholdTrace(JSONObject template, String scenarioName, EpptScenarioRun scenarioRun, NavigableMap<Double, Double> data,
										   int thresholdIndex, int gridIndex)
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
		line.put("dash", Constant.getPlotlyThresholdLineDash(thresholdIndex));
		retval.put("line", line);
		retval.put("xaxis", "x" + gridIndex);
		retval.put("yaxis", "y" + gridIndex);
		retval.put("showlegend", gridIndex == 1);
		return retval;
	}


	private JSONObject buildPrimaryTrace(JSONObject template, String scenarioName, EpptScenarioRun scenarioRun,
										 ExceedanceData exceedanceData, int gridIndex)
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
		retval.put("name", scenarioName);
		retval.put("x", xArray);
		retval.put("y", yArray);
		retval.put("xaxis", "x" + gridIndex);
		retval.put("yaxis", "y" + gridIndex);
		retval.put("marker", marker);
		retval.put("showlegend", gridIndex == 1);
		return retval;
	}

	private JSONObject buildMarker(JSONObject templateMarker, EpptScenarioRun scenarioRun)
	{
		JSONObject marker = new JSONObject(templateMarker.toString());
		Color color = scenarioRun.getColor();
		marker.put("color", Constant.colorToHex(color));
		return marker;
	}

	public static class ExceedanceMonthData
	{

		private final Map<Month, ExceedanceData> _data;

		public ExceedanceMonthData(Map<Month, ExceedanceData> data)
		{
			_data = data;
		}

		public Map<Month, ExceedanceData> getData()
		{
			return _data;
		}
	}

	@Override
	public String getPlotType()
	{
		return "Exceedance Page Plot";
	}
}

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

package gov.ca.water.reportengine.standardsummary;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.qaqc.PlotlyBubble;
import gov.ca.water.plotly.PlotlyChart;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class ScatterPlotBuilder extends PlotChartBuilder
{

	ScatterPlotBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
					   SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}

	@Override
	PlotlyChart buildChart(List<ChartComponent> chartComponents) throws EpptReportException
	{
		String title = getTitleForComponents(chartComponents);
		String yAxisLabel = getYAxisLabelForComponents(chartComponents);
		String xAxisLabel = getXAxisLabelForComponents(chartComponents);
		PlotlyBubble.BubbleData baseData = buildBubleData(getBase(), chartComponents);
		Map<EpptScenarioRun, PlotlyBubble.BubbleData> alternativeData = new HashMap<>();
		for(EpptScenarioRun alternative : getAlternatives())
		{
			alternativeData.put(alternative, buildBubleData(alternative, chartComponents));
		}
		return new PlotlyBubble(title, xAxisLabel, yAxisLabel, getBase(), baseData, alternativeData);
	}

	private PlotlyBubble.BubbleData buildBubleData(EpptScenarioRun base, List<ChartComponent> chartComponents) throws EpptReportException
	{
		List<Double> xData = new ArrayList<>();
		List<Double> yData = new ArrayList<>();
		List<String> markers = new ArrayList<>();
		if(chartComponents.size() == 2)
		{
			ChartComponent xComponent = chartComponents.get(0);
			ChartComponent yComponent = chartComponents.get(1);
			Map<Integer, Double> xValues = createJythonValueGenerator(base, xComponent.getFunction()).generateAnnualValues();
			Map<Integer, Double> yValues = createJythonValueGenerator(base, yComponent.getFunction()).generateAnnualValues();
			for(Map.Entry<Integer, Double> entry : xValues.entrySet())
			{
				Integer key = entry.getKey();
				Double xDouble = xValues.get(key);
				Double yDouble = yValues.get(key);
				if(xDouble != null && yDouble != null)
				{
					xData.add(xDouble);
					yData.add(yDouble);
					String year = String.valueOf(key);
					markers.add(year);
				}

			}
		}
		return new PlotlyBubble.BubbleData(xData, yData, markers);
	}
}

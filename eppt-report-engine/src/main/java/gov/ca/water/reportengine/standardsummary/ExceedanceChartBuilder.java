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
import java.util.NavigableMap;

import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.ExceedanceData;
import gov.ca.water.plotly.PlotlyExceedance;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
class ExceedanceChartBuilder extends PlotChartBuilder
{

	ExceedanceChartBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
						   SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}

	@Override
	PlotlyExceedance buildChart(List<ChartComponent> chartComponents) throws EpptReportException
	{
		String title = getTitleForComponents(chartComponents);
		String yAxisLabel = getYAxisLabelForComponents(chartComponents);
		String xAxisLabel = getXAxisLabelForComponents(chartComponents);
		ExceedanceData baseData = createDataForScenario(chartComponents, getBase());
		Map<EpptScenarioRun, ExceedanceData> alternativeData = new HashMap<>();
		for(EpptScenarioRun alternative : getAlternatives())
		{
			alternativeData.put(alternative, createDataForScenario(chartComponents, alternative));
		}
		return new PlotlyExceedance(title, xAxisLabel, yAxisLabel,
				getBase(), baseData, alternativeData);
	}

	private ExceedanceData createDataForScenario(List<ChartComponent> chartComponents, EpptScenarioRun scenarioRun) throws EpptReportException
	{
		ChartComponent chartComponent = chartComponents.get(0);
		NavigableMap<Double, Double> primaryData = createJythonValueGenerator(scenarioRun,
				chartComponent.getFunction()).generateExceedanceValues();
		List<NavigableMap<Double, Double>> thresholdData = new ArrayList<>();
		for(int i = 1; i < chartComponents.size(); i++)
		{
			NavigableMap<Double, Double> threshold = createJythonValueGenerator(getBase(),
					chartComponents.get(i).getFunction()).generateExceedanceValues();
			thresholdData.add(threshold);
		}
		return new ExceedanceData(primaryData, thresholdData);
	}
}

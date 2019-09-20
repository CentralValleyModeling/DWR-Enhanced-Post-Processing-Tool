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

import java.time.Month;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.PlotlyChart;
import gov.ca.water.plotly.PlotlyMonthly;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
class LinePlotBuilder extends PlotChartBuilder
{

	private static final Logger LOGGER = Logger.getLogger(LinePlotBuilder.class.getName());

	LinePlotBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
					SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}

	@Override
	PlotlyChart buildChart(List<ChartComponent> chartComponents)
	{
		String title = getTitleForComponents(chartComponents);
		String yAxisLabel = getYAxisLabelForComponents(chartComponents);
		String xAxisLabel = getXAxisLabelForComponents(chartComponents);
		Map<EpptScenarioRun, List<PlotlyMonthly.MonthlyData>> primaryData = new HashMap<>();
		Map<EpptScenarioRun, Map<String, List<PlotlyMonthly.MonthlyData>>> thresholdData = new HashMap<>();
		if(!chartComponents.isEmpty())
		{
			primaryData.put(getBase(), buildData(getBase(), chartComponents.get(0)));
			if(chartComponents.size() > 1)
			{
				thresholdData.put(getBase(), buildData(getBase(), chartComponents.subList(1, chartComponents.size())));
			}
			for(EpptScenarioRun alternative : getAlternatives())
			{
				primaryData.put(alternative, buildData(alternative, chartComponents.get(0)));
				if(chartComponents.size() > 1)
				{
					thresholdData.put(alternative, buildData(alternative, chartComponents.subList(1, chartComponents.size())));
				}
			}
		}
		return new PlotlyMonthly(title, xAxisLabel, yAxisLabel, primaryData, thresholdData);
	}

	private Map<String, List<PlotlyMonthly.MonthlyData>> buildData(EpptScenarioRun scenarioRun, List<ChartComponent> chartComponents)
	{
		return chartComponents.stream().collect(toMap(ChartComponent::getComponent, c -> buildData(scenarioRun, c)));
	}

	private List<PlotlyMonthly.MonthlyData> buildData(EpptScenarioRun base, ChartComponent chartComponent)
	{
		List<PlotlyMonthly.MonthlyData> retval = new ArrayList<>();
		try
		{
			Map<Month, Double> values = createJythonValueGenerator(base, chartComponent.getFunction()).generateMonthlyValues();
			WaterYearDefinition waterYearDefinition = getReportParameters().getWaterYearDefinition();
			for(int i = 0; i < Month.values().length; i++)
			{
				Month month = waterYearDefinition.getStartMonth().plus(i);
				Double value = values.get(month);
				if(value != null)
				{
					retval.add(new PlotlyMonthly.MonthlyData(month, value));
				}
			}
		}
		catch(EpptReportException e)
		{
			LOGGER.log(Level.SEVERE, "Error running jython script", e);
		}
		return retval;
	}
}

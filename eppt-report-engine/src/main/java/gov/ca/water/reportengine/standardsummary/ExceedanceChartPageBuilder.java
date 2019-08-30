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
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.ExceedanceData;
import gov.ca.water.plotly.PlotlyChart;
import gov.ca.water.plotly.PlotlyExceedancePage;
import gov.ca.water.reportengine.jython.MonthPeriodFilter;
import gov.ca.water.reportengine.jython.PeriodFilter;
import org.w3c.dom.Document;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
class ExceedanceChartPageBuilder extends PlotChartBuilder
{


	ExceedanceChartPageBuilder(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives,
							   SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}

	@Override
	PlotlyChart buildChart(List<ChartComponent> chartComponents)
	{
		String title = getTitleForComponents(chartComponents);
		String yAxisLabel = getYAxisLabelForComponents(chartComponents);
		Map<EpptScenarioRun, PlotlyExceedancePage.ExceedanceMonthData> exceedanceData = new HashMap<>();
		exceedanceData.put(getBase(), buildMonthData(getBase(), chartComponents));
		return new PlotlyExceedancePage(title, yAxisLabel, exceedanceData);
	}

	private PlotlyExceedancePage.ExceedanceMonthData buildMonthData(EpptScenarioRun base, List<ChartComponent> chartComponents)
	{
		EnumMap<Month, ExceedanceData> data = new EnumMap<>(Month.class);
		for(Month month : Month.values())
		{
			MonthPeriodFilter monthPeriodFilter = new MonthPeriodFilter(month);
			data.put(month, buildExceedanceData(monthPeriodFilter, base, chartComponents));
		}
		return new PlotlyExceedancePage.ExceedanceMonthData(data);
	}

	private ExceedanceData buildExceedanceData(PeriodFilter periodFilter, EpptScenarioRun base, List<ChartComponent> chartComponents)
	{
		ChartComponent chartComponent = chartComponents.get(0);
		NavigableMap<Double, Double> primaryData = createJythonValueGenerator(base, chartComponent.getFunction()).generateExceedanceValues();
		List<NavigableMap<Double, Double>> thresholdData = new ArrayList<>();
		for(int i = 1; i < chartComponents.size(); i++)
		{
			NavigableMap<Double, Double> threshold = createJythonValueGenerator(periodFilter, getBase(),
					chartComponents.get(i).getFunction()).generateExceedanceValues();
			thresholdData.add(threshold);
		}
		return new ExceedanceData(primaryData, thresholdData);
	}
}

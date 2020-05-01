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

package gov.ca.water.calgui.presentation.plotly;

import java.time.Month;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputer;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptScenarioRun;
import javafx.embed.swing.JFXPanel;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-14-2020
 */
public class PlotlyPaneBuilder
{
	private final ChartType _chartType;
	private final Map<EpptScenarioRun, List<TimeSeriesContainer>> _scenarioRunData;
	private final EpptConfigurationController _epptConfigurationController;
	private final String _plotTitle;

	public PlotlyPaneBuilder(ChartType chartType, String plotTitle, Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
							 EpptConfigurationController epptConfigurationController)
	{
		_chartType = chartType;
		_plotTitle = plotTitle;
		_scenarioRunData = scenarioRunData;
		_epptConfigurationController = epptConfigurationController;
		if(scenarioRunData.isEmpty())
		{
			throw new IllegalArgumentException("Cannot plot without scenarios");
		}
	}

	public JFXPanel build()
	{
		EpptReportingComputedSet epptReportingComputedSet;
		WaterYearDefinition waterYearDefinition = _epptConfigurationController.getWaterYearDefinition();
		List<MonthPeriod> selectedMonthlyPeriods = _epptConfigurationController.getSelectedMonthlyPeriods();
		if(_chartType == ChartType.MONTHLY_TABLE || _chartType == ChartType.SUMMARY_TABLE)
		{
			selectedMonthlyPeriods = Collections.singletonList(new MonthPeriod(null, null, null)
			{
				@Override
				public Month getStart()
				{
					return waterYearDefinition.getStartMonth();
				}

				@Override
				public Month getEnd()
				{
					return waterYearDefinition.getEndMonth();
				}

				@Override
				public String toString()
				{
					return "All Months";
				}
			});
		}
		epptReportingComputedSet = EpptReportingComputer.computeForMetrics(_scenarioRunData,
				_plotTitle, _epptConfigurationController.isTaf(), _epptConfigurationController.getWaterYearDefinition(),
				selectedMonthlyPeriods,
				_epptConfigurationController.getWaterYearPeriodRanges(),
				_epptConfigurationController.getSelectedStatistics());
		JFXPanel retval = null;
		switch(_chartType)
		{
			case TIME_SERIES_ALL:
				retval = new TimeseriesPaneDiscrete(epptReportingComputedSet);
				break;
			case TIME_SERIES_AGGREGATE:
				retval = new TimeseriesPaneAggregate(epptReportingComputedSet);
				break;
			case EXCEEDANCE_ALL:
				retval = new ExceedancePaneDiscrete(epptReportingComputedSet);
				break;
			case EXCEEDANCE_AGGREGATE:
				retval = new ExceedancePaneAggregate(epptReportingComputedSet);
				break;
			case BOX_ALL:
				retval = new BoxPlotChartPaneDiscrete(epptReportingComputedSet);
				break;
			case BOX_AGGREGATE:
				retval = new BoxPlotChartPaneAggreate(epptReportingComputedSet);
				break;
			case BAR_GRAPH:
				retval = new BarChartsPane(epptReportingComputedSet);
				break;
			case MONTHLY_LINE:
				retval = new MonthlyLinePane(epptReportingComputedSet);
				break;
			case MONTHLY_TABLE:
				retval = new MonthlyTablePane(_plotTitle, waterYearDefinition, epptReportingComputedSet, _epptConfigurationController.getSelectedMonthlyPeriods());
				break;
			case SUMMARY_TABLE:
				retval = new SummaryTablePane(_plotTitle, waterYearDefinition, epptReportingComputedSet, _epptConfigurationController.getSelectedMonthlyPeriods());
		}
		return retval;
	}

	public enum ChartType
	{
		TIME_SERIES_ALL,
		TIME_SERIES_AGGREGATE,
		EXCEEDANCE_ALL,
		MONTHLY_LINE,
		BAR_GRAPH,
		EXCEEDANCE_AGGREGATE,
		BOX_ALL,
		BOX_AGGREGATE,
		MONTHLY_TABLE,
		SUMMARY_TABLE
	}
}

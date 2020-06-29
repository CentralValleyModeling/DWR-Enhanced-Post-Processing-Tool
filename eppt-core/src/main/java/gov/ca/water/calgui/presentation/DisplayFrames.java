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

package gov.ca.water.calgui.presentation;

import java.awt.BorderLayout;
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.presentation.plotly.PlotlyPaneBuilder;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import javafx.embed.swing.JFXPanel;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-20-2020
 */
class DisplayFrames
{
	private final EpptConfigurationController _epptConfigurationController;
	private final PlotConfigurationState _plotConfigurationState;

	DisplayFrames(EpptConfigurationController epptConfigurationController,
				  PlotConfigurationState plotConfigurationState)
	{
		_epptConfigurationController = epptConfigurationController;
		_plotConfigurationState = plotConfigurationState;
	}

	Optional<String> getError()
	{
		Optional<String> retval = Optional.empty();
		List<MonthPeriod> selectedMonthlyPeriods = getEpptConfigurationController().getSelectedMonthlyPeriods();
		if(selectedMonthlyPeriods.isEmpty())
		{
			retval = Optional.of("No monthly filter selected");
		}
		else if(getEpptConfigurationController().getWaterYearPeriodRanges().isEmpty())
		{
			retval = Optional.of("No annual filter selected");
		}
		else if(getEpptConfigurationController().getSelectedStatistics().isEmpty() && getPlotConfigurationState().isDisplaySummaryTable())
		{
			retval = Optional.of("Cannot display summary table without selected statistics");
		}
		else if(getEpptConfigurationController().isDifference() && !getEpptConfigurationController().getEpptScenarioBase().isPresent())
		{
			retval = Optional.of("Cannot compute difference without Base scenario run selected");
		}
		return retval;
	}

	EpptConfigurationController getEpptConfigurationController()
	{
		return _epptConfigurationController;
	}

	PlotConfigurationState getPlotConfigurationState()
	{
		return _plotConfigurationState;
	}

	LocalDate getEnd()
	{
		int endYear = _epptConfigurationController.getEndYear();
		WaterYearDefinition waterYearDefinition = _epptConfigurationController.getWaterYearDefinition();
		WaterYearPeriod waterYearPeriod = new WaterYearPeriod("");
		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(endYear, waterYearPeriod),
				new WaterYearType(endYear, waterYearPeriod));
		YearMonth end = waterYearPeriodRange.getEnd(waterYearDefinition);
		return LocalDate.of(end.getYear(), end.getMonth(), 1).plusMonths(1).minusDays(2);
	}

	LocalDate getStart()
	{
		int startYear = _epptConfigurationController.getStartYear();
		WaterYearDefinition waterYearDefinition = _epptConfigurationController.getWaterYearDefinition();
		WaterYearPeriod waterYearPeriod = new WaterYearPeriod("");
		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(startYear, waterYearPeriod),
				new WaterYearType(startYear, waterYearPeriod));
		YearMonth start = waterYearPeriodRange.getStart(waterYearDefinition);
		return LocalDate.of(start.getYear(), start.getMonth(), 1).minusMonths(1).plusDays(2);
	}

	Optional<EpptScenarioRun> getBaseRun()
	{
		return _epptConfigurationController.getEpptScenarioBase();
	}

	List<EpptScenarioRun> getAlternatives()
	{
		return _epptConfigurationController.getEpptScenarioAlternatives();
	}

	void plotBoxPlotDiscrete(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
							 String plotTitle,
							 JTabbedPane tabbedPane)
	{

		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.BOX_ALL, plotTitle, scenarioRunData,
				_epptConfigurationController)
				.build();
		tabbedPane.addTab("Box Plot (All)", pane);
	}

	void plotBoxPlotAggregate(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
							  String plotTitle,
							  JTabbedPane tabbedPane)
	{

		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.BOX_AGGREGATE, plotTitle, scenarioRunData,
				_epptConfigurationController)
				.build();
		tabbedPane.addTab("Box Plot (Aggregate)", pane);
	}

	void plotBarCharts(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
					   String plotTitle,
					   JTabbedPane tabbedPane)
	{

		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.BAR_GRAPH, plotTitle, scenarioRunData,
				_epptConfigurationController)
				.build();
		tabbedPane.addTab("Bar Graph", pane);
	}

	void plotMonthlyLine(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						 String plotTitle,
						 JTabbedPane tabbedPane)
	{

		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.MONTHLY_LINE, plotTitle, scenarioRunData,
				_epptConfigurationController)
				.build();
		tabbedPane.addTab("Monthly Line Plot", pane);
	}

	void plotTimeSeriesDiscrete(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
								String plotTitle, JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.TIME_SERIES_ALL, plotTitle, scenarioRunData, _epptConfigurationController)
				.build();
		if(_epptConfigurationController.isDifference())
		{
			tabbedPane.addTab("Timeseries (All) - Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Timeseries (All)", pane);
		}
	}

	void plotTimeSeriesAggregate(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
								 String plotTitle, JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.TIME_SERIES_AGGREGATE, plotTitle, scenarioRunData,
				_epptConfigurationController)
				.build();
		if(_epptConfigurationController.isDifference())
		{
			tabbedPane.addTab("Timeseries (Aggregate) - Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Timeseries (Aggregate)", pane);
		}
	}

	void insertEmptyTab(JTabbedPane tabbedPane, Map<GUILinksAllModelsBO.Model, List<String>> missing)
	{
		StringBuilder buffer = new StringBuilder();
		buffer.append("<html><br>Not all DSS records were found, some results may be missing:<br><br>");
		missing.forEach((key, value) -> buffer.append("Model: ").append(key).append("<br>").append(
				String.join("<br>", value)).append("<br>"));
		buffer.append("</html>");
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(new JLabel(buffer.toString()), BorderLayout.PAGE_START);
		tabbedPane.addTab("Alert - Missing DSS records", panel);
	}

	void plotSummaryTable(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						  String plotTitle,
						  JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.SUMMARY_TABLE, plotTitle, scenarioRunData, _epptConfigurationController)
				.build();
		if(_epptConfigurationController.isDifference())
		{
			tabbedPane.addTab("Summary Table - Difference", pane);
		}
		else

		{
			tabbedPane.addTab("Summary Table", pane);
		}

	}

	void plotMonthlyTable(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						  String plotTitle,
						  JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.MONTHLY_TABLE, plotTitle, scenarioRunData, _epptConfigurationController)
				.build();
		if(_epptConfigurationController.isDifference())
		{
			tabbedPane.addTab("Monthly Table - Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Monthly Table", pane);
		}
	}

	void plotAllExceedance(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						   String plotTitle, JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE_ALL, plotTitle, scenarioRunData,
				_epptConfigurationController)
				.build();
		if(_epptConfigurationController.isDifference())
		{
			tabbedPane.addTab("Exceedance (All) - Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Exceedance (All)", pane);
		}
	}

	void plotAggregateExceedance(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
								 String plotTitle, JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE_AGGREGATE, plotTitle, scenarioRunData,
				_epptConfigurationController)
				.build();
		if(_epptConfigurationController.isDifference())
		{
			tabbedPane.addTab("Exceedance (Aggregate) - Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Exceedance (Aggregate)", pane);
		}
	}
}

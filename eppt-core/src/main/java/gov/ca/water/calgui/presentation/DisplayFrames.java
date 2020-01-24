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
import java.time.Month;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel;
import gov.ca.water.calgui.presentation.plotly.PlotlyPane;
import gov.ca.water.calgui.presentation.plotly.PlotlyPaneBuilder;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-20-2020
 */
public class DisplayFrames
{
	private final PlotConfigurationState _plotConfigurationState;
	private final EpptScenarioRun _baseRun;
	private final List<EpptScenarioRun> _alternatives;
	private final LocalDate _start;
	private final LocalDate _end;
	private final Map<EpptScenarioRun, List<WaterYearIndex>> _waterYearIndicies;

	DisplayFrames(PlotConfigurationState plotConfigurationState,
				  EpptScenarioRun baseRun,
				  List<EpptScenarioRun> alternatives,
				  LocalDate start,
				  LocalDate end) throws EpptInitializationException
	{
		_plotConfigurationState = plotConfigurationState;
		_baseRun = baseRun;
		_alternatives = alternatives;
		_start = start;
		_end = end;

		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(_baseRun);
		scenarioRuns.addAll(_alternatives);
		_waterYearIndicies = new HashMap<>();
		for(EpptScenarioRun epptScenarioRun : scenarioRuns)
		{
			WaterYearTableReader tableReader = new WaterYearTableReader(epptScenarioRun.getLookupDirectory());
			List<WaterYearIndex> read = tableReader.read();
			_waterYearIndicies.put(epptScenarioRun, read);
		}
	}

	PlotConfigurationState getPlotConfigurationState()
	{
		return _plotConfigurationState;
	}

	LocalDate getEnd()
	{
		return _end;
	}

	LocalDate getStart()
	{
		return _start;
	}

	private Map<EpptScenarioRun, List<WaterYearIndex>> getWaterYearIndicies()
	{
		return _waterYearIndicies;
	}

	EpptScenarioRun getBaseRun()
	{
		return _baseRun;
	}

	List<EpptScenarioRun> getAlternatives()
	{
		return _alternatives;
	}

	void plotBoxPlot(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
					 Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
					 String plotTitle,
					 JTabbedPane tabbedPane)
	{
		PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.BOX, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withWaterYearDefinition(new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER))
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withPlotTitle(plotTitle)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.build();
		tabbedPane.addTab("Box Plot", pane);
	}

	void plotTimeSeries(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
						String plotTitle, JTabbedPane tabbedPane)
	{
		PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.TIMESERIES, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withWaterYearDefinition(new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER))
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withPlotTitle(plotTitle)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.build();
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Time Series", pane);
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

	void plotSummaryTable(IDSSGrabber1Svc dssGrabber, DisplayInput displayInput,
						  JTabbedPane tabbedPane, String plotTitle, String sLabel, String baseRunName)
	{
		SummaryTablePanel stp;
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			stp = new SummaryTablePanel(plotTitle + " - Difference from " + displayInput.getPrimaryResults()[0].fileName,
					displayInput.getDiffResults(), null, getPlotConfigurationState().getSelectedSummaryTableItems(), "", dssGrabber);
		}
		else
		{
			stp = new SummaryTablePanel(plotTitle, displayInput.getPrimaryResults(),
					displayInput.getSecondaryResults(),
					getPlotConfigurationState().getSelectedSummaryTableItems(), sLabel, dssGrabber,
					getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
		}
		tabbedPane.addTab("Summary - " + baseRunName, stp);
	}

	void plotMonthlyTable(IDSSGrabber1Svc dssGrabber, DisplayInput displayInput,
						  JTabbedPane tabbedPane, String plotTitle, String sLabel, String baseRunName)
	{
		MonthlyTablePanel mtp;
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			mtp = new MonthlyTablePanel(
					plotTitle + " - Difference from " + displayInput.getPrimaryResults()[0].fileName,
					displayInput.getDiffResults(), null, dssGrabber, "");
		}
		else
		{
			mtp = new MonthlyTablePanel(plotTitle, displayInput.getPrimaryResults(),
					displayInput.getSecondaryResults(),
					dssGrabber, sLabel,
					getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
		}
		tabbedPane.addTab("Monthly - " + baseRunName, mtp);
	}

	void plotExceedance(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
						String plotTitle, JTabbedPane tabbedPane)
	{
		if(getPlotConfigurationState().isPlotAllExceedancePlots())
		{
			plotAllExceedance(scenarioRunData, secondaryScenarioRunData, plotTitle, tabbedPane);
		}
		if(getPlotConfigurationState().isAnnualFlowExceedancePlots())
		{
			plotAnnualExceedance(scenarioRunData, secondaryScenarioRunData, plotTitle, tabbedPane);
		}
		if(!getPlotConfigurationState().getSelectedExceedancePlotMonths().isEmpty())
		{
			plotMonthlyExceedance(scenarioRunData, secondaryScenarioRunData, plotTitle, tabbedPane);
		}
	}

	private void plotMonthlyExceedance(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
									   Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
									   String plotTitle, JTabbedPane tabbedPane)
	{
		List<Month> exceedMonths = getPlotConfigurationState().getSelectedExceedancePlotMonths();
		for(Month month : exceedMonths)
		{
			PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
					.withComparisonType(getPlotConfigurationState().getComparisonType())
					.withTaf(getPlotConfigurationState().isDisplayTaf())
					.withPlotTitle(plotTitle)
					.withTimeWindow(getStart(), getEnd())
					.withWaterYearIndicies(getWaterYearIndicies())
					.withMonth(month)
					.build();
			tabbedPane.addTab("Exceedance (" + month.getDisplayName(TextStyle.SHORT, Locale.getDefault()) + ")", pane);
		}
	}

	private void plotAllExceedance(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
								   Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
								   String plotTitle, JTabbedPane tabbedPane)
	{
		PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withWaterYearDefinition(new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER))
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withPlotTitle(plotTitle)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.build();
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Exceedance (All - Difference)", pane);
		}
		else
		{
			tabbedPane.addTab("Exceedance (All)", pane);
		}
	}

	private void plotAnnualExceedance(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
									  Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
									  String plotTitle, JTabbedPane tabbedPane)
	{
		WaterYearDefinition waterYearDefinition = new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER);
		PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withPlotTitle(plotTitle)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.withWaterYearDefinition(waterYearDefinition)
				.build();
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Exceedance (Annual Total - Difference)", pane);
		}
		else
		{
			tabbedPane.addTab("Exceedance (Annual Total)", pane);
		}
	}
}

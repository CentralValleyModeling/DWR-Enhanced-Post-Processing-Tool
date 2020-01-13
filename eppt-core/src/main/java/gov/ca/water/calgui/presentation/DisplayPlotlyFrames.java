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

package gov.ca.water.calgui.presentation;

import java.awt.BorderLayout;
import java.time.LocalDate;
import java.time.Month;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel;
import gov.ca.water.calgui.presentation.plotly.AllExceedancePane;
import gov.ca.water.calgui.presentation.plotly.AnnualExceedancePane;
import gov.ca.water.calgui.presentation.plotly.BoxPlotChartPane;
import gov.ca.water.calgui.presentation.plotly.MonthExceedancePane;
import gov.ca.water.calgui.presentation.plotly.TimeseriesChartPane;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

import hec.io.TimeSeriesContainer;

import static java.util.stream.Collectors.toList;

/**
 * DisplayFrame class provides a frame for showing charts.
 *
 * @author tslawecki
 */
final class DisplayPlotlyFrames
{

	private static final Logger LOG = Logger.getLogger(DisplayPlotlyFrames.class.getName());
	private static final IErrorHandlingSvc ERROR_HANDLING_SVC = new ErrorHandlingSvcImpl();
	private final PlotConfigurationState _plotConfigurationState;
	private final List<GUILinksAllModelsBO> _guiLinks;
	private final EpptScenarioRun _baseRun;
	private final List<EpptScenarioRun> _alternatives;
	private final LocalDate _start;
	private final LocalDate _end;

	DisplayPlotlyFrames(PlotConfigurationState plotConfigurationState,
						List<GUILinksAllModelsBO> guiLinks,
						EpptScenarioRun baseRun,
						List<EpptScenarioRun> alternatives,
						LocalDate start,
						LocalDate end)
	{
		_plotConfigurationState = plotConfigurationState;
		_guiLinks = guiLinks;
		_baseRun = baseRun;
		_alternatives = alternatives;
		_start = start;
		_end = end;
	}

	/**
	 * showDisplayFrames method creates a frame showing multiple charts
	 * according to parameters.
	 */
	List<JTabbedPane> showDisplayFrames()
	{
		List<JTabbedPane> tabbedPanes = new ArrayList<>();
		try
		{

			IDSSGrabber1Svc dssGrabber = new DSSGrabber1SvcImpl();
			dssGrabber.setIsCFS(!_plotConfigurationState.isDisplayTaf());
			dssGrabber.setScenarioRuns(_baseRun, _alternatives);

			if(_guiLinks.isEmpty())
			{
				String message = "No Location selected.";
				ERROR_HANDLING_SVC.businessErrorHandler(message, message);
			}
			for(GUILinksAllModelsBO guiLink : _guiLinks)
			{
				displayForLocation(_plotConfigurationState, _baseRun, _start, _end,
						tabbedPanes, dssGrabber, guiLink);
			}
		}
		catch(RuntimeException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display frame.";
			ERROR_HANDLING_SVC.businessErrorHandler(messageText, e);
		}
		return tabbedPanes;
	}

	private void displayForLocation(PlotConfigurationState plotConfigurationState, EpptScenarioRun baseRun, LocalDate start,
									LocalDate end, List<JTabbedPane> tabbedPanes, IDSSGrabber1Svc dssGrabber, GUILinksAllModelsBO guiLink)
	{
		dssGrabber.setGuiLink(guiLink);
		if(dssGrabber.getPrimaryDSSName() == null)
		{
			String message = "No GUI_Links3.csv entry found for " + guiLink + "/" + guiLink + ".";
			ERROR_HANDLING_SVC.businessErrorHandler(message, message);
		}
		else if(dssGrabber.getPrimaryDSSName().isEmpty())
		{
			String message = "No DSS time series specified for " + guiLink + "/" + guiLink + ".";
			ERROR_HANDLING_SVC.businessErrorHandler(message, message);
		}
		else
		{
			dssGrabber.setDateRange(start, end);
			TimeSeriesContainer[] primaryResults = dssGrabber.getPrimarySeries();
			TimeSeriesContainer[] secondaryResults = dssGrabber.getSecondarySeries();
			dssGrabber.calcTAFforCFS(primaryResults, secondaryResults);
			TimeSeriesContainer[] diffResults = dssGrabber.getDifferenceSeries(primaryResults);
			TimeSeriesContainer[][] excResults = dssGrabber.getExceedanceSeries(primaryResults);
			TimeSeriesContainer[][] sexcResults = dssGrabber.getExceedanceSeries(secondaryResults);
			TimeSeriesContainer[][] dexcResults = dssGrabber.getExceedanceSeriesD(primaryResults);
			JTabbedPane tabbedpane = displayFrameForData(plotConfigurationState, baseRun, dssGrabber,
					new DisplayInput(guiLink, primaryResults, secondaryResults, diffResults, excResults, sexcResults, dexcResults));

			tabbedPanes.add(tabbedpane);
		}
	}

	private JTabbedPane displayFrameForData(PlotConfigurationState plotConfigurationState,
											EpptScenarioRun baseRun,
											IDSSGrabber1Svc dssGrabber,
											DisplayInput displayInput)
	{
		JTabbedPane tabbedPane = new JTabbedPane();
		String plotTitle = dssGrabber.getPlotTitle();
		String sLabel = dssGrabber.getSLabel();
		String baseRunName = dssGrabber.getBaseRunName();
		Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
		GUILinksAllModelsBO guiLink = displayInput.getGuiLink();
		if(displayInput.getPrimaryResults() != null && displayInput.getPrimaryResults()[0] != null)
		{
			if(plotConfigurationState.isDisplayTimeSeriesPlot())
			{
				plotTimeSeries(plotConfigurationState, guiLink, tabbedPane);
			}
			if(plotConfigurationState.isDoExceedance())
			{
				plotExceedance(plotConfigurationState, tabbedPane, guiLink);
			}
			if(plotConfigurationState.isDisplayBoxAndWhiskerPlot())
			{
				plotBoxPlot(tabbedPane, guiLink);
			}
			if(plotConfigurationState.isDisplayMonthlyTable())
			{
				plotMonthlyTable(plotConfigurationState, dssGrabber, displayInput, tabbedPane, plotTitle, sLabel, baseRunName);
			}
			if(plotConfigurationState.isDisplaySummaryTable())
			{
				plotSummaryTable(plotConfigurationState, dssGrabber, displayInput, tabbedPane, plotTitle, sLabel, baseRunName);
			}
		}
		List<String> collect = missing.values()
									  .stream()
									  .flatMap(Collection::stream)
									  .collect(toList());
		if(!collect.isEmpty())
		{
			insertEmptyTab(tabbedPane, missing);
		}
		String title = baseRun.getName() + " - " + guiLink.getPlotTitle();
		tabbedPane.setName(title);
		return tabbedPane;
	}

	private void plotTimeSeries(PlotConfigurationState plotConfigurationState, GUILinksAllModelsBO guiLink, JTabbedPane tabbedPane)
	{
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(_baseRun);
		scenarioRuns.addAll(_alternatives);
		boolean taf = _plotConfigurationState.isDisplayTaf();
		TimeseriesChartPane cp2 = new TimeseriesChartPane(guiLink, scenarioRuns, plotConfigurationState.getComparisonType(),
				_start, _end, taf);
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Difference", cp2);
		}
		else
		{
			tabbedPane.addTab("Time Series", cp2);
		}
	}

	private void plotExceedance(PlotConfigurationState plotConfigurationState, JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		if(!plotConfigurationState.getSelectedExceedancePlotMonths().isEmpty())
		{
			plotMonthlyExceedance(plotConfigurationState, tabbedPane, guiLink);
		}
		if(plotConfigurationState.isAnnualFlowExceedancePlots())
		{
			plotAnnualExceedance(tabbedPane, guiLink);
		}
		if(plotConfigurationState.isPlotAllExceedancePlots())
		{
			plotAllExceedance(plotConfigurationState, tabbedPane, guiLink);
		}
	}

	private void plotMonthlyExceedance(PlotConfigurationState plotConfigurationState, JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		List<Month> exceedMonths = plotConfigurationState.getSelectedExceedancePlotMonths();
		for(Month month : exceedMonths)
		{
			List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
			scenarioRuns.add(_baseRun);
			scenarioRuns.addAll(_alternatives);
			boolean taf = _plotConfigurationState.isDisplayTaf();
			final JComponent cp3 = new MonthExceedancePane(month, guiLink, scenarioRuns, plotConfigurationState.getComparisonType(), _start, _end,
					taf);
			tabbedPane.addTab("Exceedance (" + month.getDisplayName(TextStyle.SHORT, Locale.getDefault()) + ")", cp3);
		}
	}

	private void plotAllExceedance(PlotConfigurationState plotConfigurationState, JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(_baseRun);
		scenarioRuns.addAll(_alternatives);
		boolean taf = _plotConfigurationState.isDisplayTaf();
		AllExceedancePane allExceedancePane = new AllExceedancePane(guiLink, scenarioRuns, plotConfigurationState.getComparisonType(),
				_start, _end, taf);
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.insertTab("Exceedance (All - Difference)", null, allExceedancePane, null, 0);
		}
		else
		{
			tabbedPane.insertTab("Exceedance (All)", null, allExceedancePane, null, 0);
		}
	}

	private void plotAnnualExceedance(JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(_baseRun);
		scenarioRuns.addAll(_alternatives);
		boolean taf = _plotConfigurationState.isDisplayTaf();
		WaterYearDefinition waterYearDefinition = new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER);
		AnnualExceedancePane cp3 = new AnnualExceedancePane(waterYearDefinition, guiLink, scenarioRuns, _start, _end, taf);
		if(_plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Exceedance (Annual Total - Difference)", cp3);
		}
		else
		{
			tabbedPane.addTab("Exceedance (Annual Total)", cp3);
		}
	}

	private void plotBoxPlot(JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(_baseRun);
		scenarioRuns.addAll(_alternatives);
		boolean taf = _plotConfigurationState.isDisplayTaf();
		BoxPlotChartPane boxPlotChartPane = new BoxPlotChartPane(guiLink, scenarioRuns, _start, _end, taf);
		tabbedPane.addTab("Box Plot", boxPlotChartPane);
	}

	private void plotMonthlyTable(PlotConfigurationState plotConfigurationState, IDSSGrabber1Svc dssGrabber, DisplayInput displayInput,
								  JTabbedPane tabbedPane, String plotTitle, String sLabel, String baseRunName)
	{
		MonthlyTablePanel mtp;
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
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
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
		}
		tabbedPane.addTab("Monthly - " + baseRunName, mtp);
	}

	private void plotSummaryTable(PlotConfigurationState plotConfigurationState, IDSSGrabber1Svc dssGrabber, DisplayInput displayInput,
								  JTabbedPane tabbedPane, String plotTitle, String sLabel, String baseRunName)
	{
		SummaryTablePanel stp;
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			stp = new SummaryTablePanel(plotTitle + " - Difference from " + displayInput.getPrimaryResults()[0].fileName,
					displayInput.getDiffResults(), null, plotConfigurationState.getSelectedSummaryTableItems(), "", dssGrabber);
		}
		else
		{
			stp = new SummaryTablePanel(plotTitle, displayInput.getPrimaryResults(),
					displayInput.getSecondaryResults(),
					plotConfigurationState.getSelectedSummaryTableItems(), sLabel, dssGrabber,
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
		}
		tabbedPane.addTab("Summary - " + baseRunName, stp);
	}

	private void insertEmptyTab(JTabbedPane tabbedPane, Map<GUILinksAllModelsBO.Model, List<String>> missing)
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

}

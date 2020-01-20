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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel;
import gov.ca.water.calgui.presentation.plotly.PlotlyPane;
import gov.ca.water.calgui.presentation.plotly.PlotlyPaneBuilder;
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
				displayForLocation(tabbedPanes, dssGrabber, guiLink);
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

	private void displayForLocation(List<JTabbedPane> tabbedPanes, IDSSGrabber1Svc dssGrabber, GUILinksAllModelsBO guiLink)
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
			dssGrabber.setDateRange(_start, _end);
			TimeSeriesContainer[] primaryResults = dssGrabber.getPrimarySeries();
			TimeSeriesContainer[] secondaryResults = dssGrabber.getSecondarySeries();
			dssGrabber.calcTAFforCFS(primaryResults, secondaryResults);
			TimeSeriesContainer[] diffResults = dssGrabber.getDifferenceSeries(primaryResults);
			TimeSeriesContainer[][] excResults = dssGrabber.getExceedanceSeries(primaryResults);
			TimeSeriesContainer[][] sexcResults = dssGrabber.getExceedanceSeries(secondaryResults);
			TimeSeriesContainer[][] dexcResults = dssGrabber.getExceedanceSeriesD(primaryResults);
			try
			{
				JTabbedPane tabbedPane = displayFrameForData(dssGrabber,
						new DisplayInput(guiLink, primaryResults, secondaryResults, diffResults, excResults, sexcResults, dexcResults));
				tabbedPanes.add(tabbedPane);
			}
			catch(EpptInitializationException e)
			{
				ERROR_HANDLING_SVC.businessErrorHandler("Error plotting data for GUILink: " + guiLink.getCheckboxId(), e);
			}

		}
	}

	private JTabbedPane displayFrameForData(IDSSGrabber1Svc dssGrabber, DisplayInput displayInput) throws EpptInitializationException
	{
		JTabbedPane tabbedPane = new JTabbedPane();
		String plotTitle = dssGrabber.getPlotTitle();
		String sLabel = dssGrabber.getSLabel();
		String baseRunName = dssGrabber.getBaseRunName();
		Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
		GUILinksAllModelsBO guiLink = displayInput.getGuiLink();
		if(displayInput.getPrimaryResults() != null && displayInput.getPrimaryResults()[0] != null)
		{
			List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
			scenarioRuns.add(_baseRun);
			scenarioRuns.addAll(_alternatives);
			Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
			for(EpptScenarioRun epptScenarioRun : scenarioRuns)
			{
				TimeSeriesContainer[] primarySeries = buildDssGrabber(epptScenarioRun, guiLink, _plotConfigurationState.isDisplayTaf(), _start,
						_end).getPrimarySeries();
				scenarioRunData.put(epptScenarioRun, primarySeries);
			}
			Map<EpptScenarioRun, List<WaterYearIndex>> waterYearIndicies = new HashMap<>();
			for(EpptScenarioRun epptScenarioRun : scenarioRuns)
			{
				WaterYearTableReader tableReader = new WaterYearTableReader(epptScenarioRun.getLookupDirectory());
				List<WaterYearIndex> read = tableReader.read();
				waterYearIndicies.put(epptScenarioRun, read);
			}
			if(_plotConfigurationState.isDisplayTimeSeriesPlot())
			{
				plotTimeSeries(scenarioRunData, waterYearIndicies, _plotConfigurationState, guiLink, tabbedPane);
			}
			if(_plotConfigurationState.isDoExceedance())
			{
				plotExceedance(scenarioRunData, waterYearIndicies, _plotConfigurationState, tabbedPane, guiLink);
			}
			if(_plotConfigurationState.isDisplayBoxAndWhiskerPlot())
			{
				plotBoxPlot(scenarioRunData, waterYearIndicies, tabbedPane, guiLink);
			}
			if(_plotConfigurationState.isDisplayMonthlyTable())
			{
				plotMonthlyTable(_plotConfigurationState, dssGrabber, displayInput, tabbedPane, plotTitle, sLabel, baseRunName);
			}
			if(_plotConfigurationState.isDisplaySummaryTable())
			{
				plotSummaryTable(_plotConfigurationState, dssGrabber, displayInput, tabbedPane, plotTitle, sLabel, baseRunName);
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
		String title = guiLink.getPlotTitle();
		tabbedPane.setName(title);
		return tabbedPane;
	}

	private void plotTimeSeries(Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
								Map<EpptScenarioRun, List<WaterYearIndex>> waterYearIndicies,
								PlotConfigurationState plotConfigurationState, GUILinksAllModelsBO guiLink, JTabbedPane tabbedPane)
	{
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(_baseRun);
		scenarioRuns.addAll(_alternatives);
		PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.TIMESERIES, _baseRun, scenarioRunData)
				.withComparisonType(plotConfigurationState.getComparisonType())
				.withWaterYearDefinition(new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER))
				.withTaf(_plotConfigurationState.isDisplayTaf())
				.withGuiLink(guiLink)
				.withTimeWindow(_start, _end)
				.withWaterYearIndicies(waterYearIndicies)
				.build();
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Time Series", pane);
		}
	}

	private void plotExceedance(Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
								Map<EpptScenarioRun, List<WaterYearIndex>> waterYearIndicies,
								PlotConfigurationState plotConfigurationState, JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		if(plotConfigurationState.isPlotAllExceedancePlots())
		{
			plotAllExceedance(scenarioRunData, waterYearIndicies, plotConfigurationState, tabbedPane, guiLink);
		}
		if(plotConfigurationState.isAnnualFlowExceedancePlots())
		{
			plotAnnualExceedance(scenarioRunData, waterYearIndicies, tabbedPane, guiLink);
		}
		if(!plotConfigurationState.getSelectedExceedancePlotMonths().isEmpty())
		{
			plotMonthlyExceedance(scenarioRunData, waterYearIndicies, plotConfigurationState, tabbedPane, guiLink);
		}
	}

	private void plotMonthlyExceedance(Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
									   Map<EpptScenarioRun, List<WaterYearIndex>> waterYearIndicies,
									   PlotConfigurationState plotConfigurationState, JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		List<Month> exceedMonths = plotConfigurationState.getSelectedExceedancePlotMonths();
		for(Month month : exceedMonths)
		{
			PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, _baseRun, scenarioRunData)
					.withComparisonType(plotConfigurationState.getComparisonType())
					.withTaf(_plotConfigurationState.isDisplayTaf())
					.withGuiLink(guiLink)
					.withTimeWindow(_start, _end)
					.withWaterYearIndicies(waterYearIndicies)
					.withMonth(month)
					.build();
			tabbedPane.addTab("Exceedance (" + month.getDisplayName(TextStyle.SHORT, Locale.getDefault()) + ")", pane);
		}
	}

	private void plotAllExceedance(Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
								   Map<EpptScenarioRun, List<WaterYearIndex>> waterYearIndicies,
								   PlotConfigurationState plotConfigurationState, JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, _baseRun, scenarioRunData)
				.withTaf(_plotConfigurationState.isDisplayTaf())
				.withWaterYearDefinition(new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER))
				.withComparisonType(plotConfigurationState.getComparisonType())
				.withGuiLink(guiLink)
				.withTimeWindow(_start, _end)
				.withWaterYearIndicies(waterYearIndicies)
				.build();
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Exceedance (All - Difference)", pane);
		}
		else
		{
			tabbedPane.addTab("Exceedance (All)", pane);
		}
	}

	private void plotAnnualExceedance(Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
									  Map<EpptScenarioRun, List<WaterYearIndex>> waterYearIndicies,
									  JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		WaterYearDefinition waterYearDefinition = new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER);
		PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, _baseRun, scenarioRunData)
				.withComparisonType(_plotConfigurationState.getComparisonType())
				.withTaf(_plotConfigurationState.isDisplayTaf())
				.withGuiLink(guiLink)
				.withTimeWindow(_start, _end)
				.withWaterYearIndicies(waterYearIndicies)
				.withWaterYearDefinition(waterYearDefinition)
				.build();
		if(_plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Exceedance (Annual Total - Difference)", pane);
		}
		else
		{
			tabbedPane.addTab("Exceedance (Annual Total)", pane);
		}
	}

	private void plotBoxPlot(Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData,
							 Map<EpptScenarioRun, List<WaterYearIndex>> waterYearIndicies,
							 JTabbedPane tabbedPane, GUILinksAllModelsBO guiLink)
	{
		PlotlyPane pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.BOX, _baseRun, scenarioRunData)
				.withComparisonType(_plotConfigurationState.getComparisonType())
				.withWaterYearDefinition(new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER))
				.withTaf(_plotConfigurationState.isDisplayTaf())
				.withGuiLink(guiLink)
				.withTimeWindow(_start, _end)
				.withWaterYearIndicies(waterYearIndicies)
				.build();
		tabbedPane.addTab("Box Plot", pane);
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

	private DSSGrabber1SvcImpl buildDssGrabber(EpptScenarioRun epptScenarioRun, GUILinksAllModelsBO guiLink, boolean isCFS, LocalDate start,
											   LocalDate end)
	{
		DSSGrabber1SvcImpl dssGrabber = new DSSGrabber1SvcImpl();
		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(epptScenarioRun, Collections.emptyList());
		dssGrabber.setGuiLink(guiLink);
		dssGrabber.setDateRange(start, end);
		return dssGrabber;
	}

}

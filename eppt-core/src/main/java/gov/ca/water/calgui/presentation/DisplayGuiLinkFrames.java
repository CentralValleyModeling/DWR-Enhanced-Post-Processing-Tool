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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
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
final class DisplayGuiLinkFrames extends DisplayFrames
{

	private static final Logger LOG = Logger.getLogger(DisplayGuiLinkFrames.class.getName());
	private static final IErrorHandlingSvc ERROR_HANDLING_SVC = new ErrorHandlingSvcImpl();
	private final List<GUILinksAllModelsBO> _guiLinks;

	DisplayGuiLinkFrames(PlotConfigurationState plotConfigurationState,
						 List<GUILinksAllModelsBO> guiLinks,
						 EpptScenarioRun baseRun,
						 List<EpptScenarioRun> alternatives,
						 LocalDate start,
						 LocalDate end) throws EpptInitializationException
	{
		super(plotConfigurationState, baseRun, alternatives, start, end);
		_guiLinks = guiLinks;
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
			dssGrabber.setIsCFS(!getPlotConfigurationState().isDisplayTaf());
			dssGrabber.setScenarioRuns(getBaseRun(), getAlternatives());

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
			dssGrabber.setDateRange(getStart(), getEnd());
			TimeSeriesContainer[] primaryResults = dssGrabber.getPrimarySeries();
			TimeSeriesContainer[] secondaryResults = dssGrabber.getSecondarySeries();
			dssGrabber.calcTAFforCFS(primaryResults, secondaryResults);
			TimeSeriesContainer[] diffResults = dssGrabber.getDifferenceSeries(primaryResults);
			JTabbedPane tabbedPane = displayFrameForData(dssGrabber, guiLink, new DisplayInput(primaryResults, secondaryResults, diffResults));
			tabbedPanes.add(tabbedPane);

		}
	}

	private JTabbedPane displayFrameForData(IDSSGrabber1Svc dssGrabber, GUILinksAllModelsBO guiLink, DisplayInput displayInput)
	{
		JTabbedPane tabbedPane = new JTabbedPane();
		String plotTitle = dssGrabber.getPlotTitle();
		String sLabel = dssGrabber.getSLabel();
		String baseRunName = dssGrabber.getBaseRunName();
		Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
		if(displayInput.getPrimaryResults() != null && displayInput.getPrimaryResults()[0] != null)
		{
			List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
			scenarioRuns.add(getBaseRun());
			scenarioRuns.addAll(getAlternatives());
			Map<EpptScenarioRun, List<TimeSeriesContainer>> primaryScenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
			Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
			for(EpptScenarioRun epptScenarioRun : scenarioRuns)
			{
				DSSGrabber1SvcImpl dssGrabber1Svc = buildDssGrabber(epptScenarioRun, guiLink, getPlotConfigurationState().isDisplayTaf(),
						getStart(),
						getEnd());
				TimeSeriesContainer primarySeries = dssGrabber1Svc.getPrimarySeries()[0];
				TimeSeriesContainer secondarySeries = dssGrabber1Svc.getSecondarySeries()[0];
				primaryScenarioRunData.put(epptScenarioRun, Collections.singletonList(primarySeries));
				secondaryScenarioRunData.put(epptScenarioRun, Collections.singletonList(secondarySeries));
			}
			if(getPlotConfigurationState().isDisplayTimeSeriesPlot())
			{
				plotTimeSeries(primaryScenarioRunData, secondaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
			}
			if(getPlotConfigurationState().isDoExceedance())
			{
				plotExceedance(primaryScenarioRunData, secondaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
			}
			if(getPlotConfigurationState().isDisplayBoxAndWhiskerPlot())
			{
				plotBoxPlot(primaryScenarioRunData, secondaryScenarioRunData, guiLink.getPlotTitle(), tabbedPane);
			}
			if(getPlotConfigurationState().isDisplayMonthlyTable())
			{
				plotMonthlyTable(dssGrabber, displayInput, tabbedPane, plotTitle, sLabel, baseRunName);
			}
			if(getPlotConfigurationState().isDisplaySummaryTable())
			{
				plotSummaryTable(dssGrabber, displayInput, tabbedPane, plotTitle, sLabel, baseRunName);
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

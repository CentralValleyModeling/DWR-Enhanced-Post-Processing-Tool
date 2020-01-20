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

import java.awt.Component;
import java.awt.Cursor;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import javax.swing.*;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.presentation.display.DefaultPlotHandler;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import org.apache.log4j.Logger;

import static java.util.stream.Collectors.toList;

public class DisplayHelper
{
	private static final Logger LOGGER = Logger.getLogger(DisplayHelper.class.getName());
	private static PlotHandler topComponentPlotHandler = new DefaultPlotHandler();
	private final ExecutorService _executorService;
	private final Component _component;

	public DisplayHelper(Component comp)
	{
		_component = comp;
		_executorService = Executors.newSingleThreadExecutor(DisplayHelper::newThread);
	}

	public static void installPlotHandler(PlotHandler plotHandler)
	{
		topComponentPlotHandler = plotHandler;
	}

	private static Thread newThread(Runnable r)
	{
		return new Thread(r, "DisplayHelperThread");
	}

	public void showDisplayFramesLocations(PlotConfigurationState plotConfigurationState, List<String> locations,
										   EpptScenarioRun baseRun, List<EpptScenarioRun> scenarios,
										   LocalDate startMonth, LocalDate endMonth)
	{
		displayFramesOnBackgroundLocations(plotConfigurationState, locations, baseRun, scenarios, startMonth, endMonth);
	}

	public void showDisplayFramesGuiLink(PlotConfigurationState plotConfigurationState, List<GUILinksAllModelsBO> locations, EpptScenarioRun baseRun,
										 List<EpptScenarioRun> scenarios,
										 LocalDate startMonth,
										 LocalDate endMonth)
	{
		displayFramesOnBackgroundGuiLink(plotConfigurationState, locations, baseRun, scenarios, startMonth, endMonth);
	}

	public void showDisplayFramesWRIMS(PlotConfigurationState plotConfigurationState, EpptScenarioRun baseRun, List<EpptScenarioRun> lstScenarios,
									   DerivedTimeSeries dts,
									   MultipleTimeSeries mts, LocalDate startMonth, LocalDate endMonth)
	{
		displayFramesWRIMSOnBackground(plotConfigurationState, baseRun, lstScenarios, dts, mts, startMonth, endMonth);
	}


	private void displayFramesOnBackgroundLocations(PlotConfigurationState plotConfigurationState, List<String> locations, EpptScenarioRun baseRun,
													List<EpptScenarioRun> scenarios, LocalDate startMonth,
													LocalDate endMonth)
	{
		CompletableFuture.supplyAsync(() -> getTabbedPanesLocations(plotConfigurationState, locations, baseRun, scenarios, startMonth, endMonth),
				_executorService)
						 .thenAcceptAsync(tabbedPanes -> topComponentPlotHandler.openPlots(tabbedPanes), SwingUtilities::invokeLater);


	}


	private void displayFramesOnBackgroundGuiLink(PlotConfigurationState plotConfigurationState, List<GUILinksAllModelsBO> locations,
												  EpptScenarioRun baseRun,
												  List<EpptScenarioRun> scenarios, LocalDate startMonth,
												  LocalDate endMonth)
	{
		CompletableFuture.supplyAsync(() -> getTabbedPanesGuiLink(plotConfigurationState, locations, baseRun, scenarios, startMonth, endMonth),
				_executorService)
						 .thenAcceptAsync(tabbedPanes -> topComponentPlotHandler.openPlots(tabbedPanes), SwingUtilities::invokeLater);


	}

	private List<JTabbedPane> getTabbedPanesLocations(PlotConfigurationState plotConfigurationState, List<String> locations, EpptScenarioRun baseRun,
													  List<EpptScenarioRun> scenarios, LocalDate startMonth, LocalDate endMonth)
	{
		List<JTabbedPane> jTabbedPanes = new ArrayList<>();
		try
		{
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			List<GUILinksAllModelsBO> guiLinks = locations.stream()
														  .map(s ->
														  {
															  GUILinksAllModelsBO guiLinksAllModelsBO = new GUILinksAllModelsBO(
																	  s, s,
																	  s, s);
															  GUILinksAllModelsBO.Model.values().forEach(
																	  m -> guiLinksAllModelsBO.addModelMapping(m.toString(), s, ""));
															  return guiLinksAllModelsBO;
														  }).collect(toList());
			DisplayPlotlyFrames displayPlotlyFrames = new DisplayPlotlyFrames(plotConfigurationState, guiLinks, baseRun,
					scenarios, startMonth,
					endMonth);
			jTabbedPanes.addAll(displayPlotlyFrames.showDisplayFrames());
		}
		catch(RuntimeException ex)
		{
			LOGGER.error("An error occurred while trying to display results", ex);
		}
		finally
		{

			setCursor(Cursor.getDefaultCursor());
		}
		return jTabbedPanes;
	}

	private List<JTabbedPane> getTabbedPanesGuiLink(PlotConfigurationState plotConfigurationState, List<GUILinksAllModelsBO> locations,
													EpptScenarioRun baseRun,
													List<EpptScenarioRun> scenarios, LocalDate startMonth, LocalDate endMonth)
	{
		List<JTabbedPane> jTabbedPanes = new ArrayList<>();
		try
		{
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			DisplayPlotlyFrames displayPlotlyFrames = new DisplayPlotlyFrames(plotConfigurationState, locations, baseRun, scenarios, startMonth,
					endMonth);
			jTabbedPanes.addAll(displayPlotlyFrames.showDisplayFrames());
		}
		catch(RuntimeException ex)
		{
			LOGGER.error("An error occurred while trying to display results", ex);
		}
		finally
		{

			setCursor(Cursor.getDefaultCursor());
		}
		return jTabbedPanes;
	}

	private void setCursor(Cursor cursor)
	{
		SwingUtilities.invokeLater(() -> _component.setCursor(cursor));
	}

	private void displayFramesWRIMSOnBackground(PlotConfigurationState plotConfigurationState, EpptScenarioRun baseRun,
												List<EpptScenarioRun> lstScenarios, DerivedTimeSeries dts,
												MultipleTimeSeries mts, LocalDate startMonth, LocalDate endMonth)
	{
		CompletableFuture.supplyAsync(
				() -> getTabbedPanesWRIMS(plotConfigurationState, baseRun, lstScenarios, dts, mts, startMonth, endMonth), _executorService)
						 .thenAcceptAsync(topComponentPlotHandler::openPlots, SwingUtilities::invokeLater);


	}

	private List<JTabbedPane> getTabbedPanesWRIMS(PlotConfigurationState plotConfigurationState, EpptScenarioRun baseRun,
												  List<EpptScenarioRun> lstScenarios, DerivedTimeSeries dts,
												  MultipleTimeSeries mts, LocalDate startMonth, LocalDate endMonth)
	{
		List<JTabbedPane> jTabbedPanes = new ArrayList<>();
		try
		{
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			jTabbedPanes = DisplayFrame.showDisplayFramesWRIMS(plotConfigurationState, baseRun, lstScenarios, dts, mts,
					startMonth, endMonth);
		}
		catch(RuntimeException ex)
		{
			LOGGER.error("An error occurred while trying to display results", ex);
		}
		finally
		{
			setCursor(Cursor.getDefaultCursor());
		}
		return jTabbedPanes;

	}


	@FunctionalInterface
	public interface PlotHandler
	{
		void openPlots(List<JTabbedPane> tabbedPanes);
	}

}

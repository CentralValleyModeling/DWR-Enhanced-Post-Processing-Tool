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
import java.util.function.Consumer;
import javax.swing.*;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.EpptInitializationException;
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


	private void displayFramesOnBackgroundGuiLink(PlotConfigurationState plotConfigurationState, List<GUILinksAllModelsBO> locations,
												  EpptScenarioRun baseRun,
												  List<EpptScenarioRun> scenarios, LocalDate startMonth,
												  LocalDate endMonth)
	{
		CompletableFuture.supplyAsync(() -> getTabbedPanesGuiLink(plotConfigurationState, locations, baseRun, scenarios, startMonth, endMonth),
				_executorService)
						 .thenAcceptAsync(tabbedPanes -> topComponentPlotHandler.openPlots(tabbedPanes), SwingUtilities::invokeLater);


	}

	private List<JTabbedPane> getTabbedPanesGuiLink(PlotConfigurationState plotConfigurationState, List<GUILinksAllModelsBO> locations,
													EpptScenarioRun baseRun,
													List<EpptScenarioRun> scenarios, LocalDate startMonth, LocalDate endMonth)
	{
		List<JTabbedPane> jTabbedPanes = new ArrayList<>();
		try
		{
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

			DisplayGuiLinkFrames displayGuiLinkFrames = new DisplayGuiLinkFrames(plotConfigurationState, locations, baseRun, scenarios, startMonth,
					endMonth);
			jTabbedPanes.addAll(displayGuiLinkFrames.showDisplayFrames());
		}
		catch(RuntimeException | EpptInitializationException ex)
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
												  List<EpptScenarioRun> scenarios, DerivedTimeSeries dts,
												  MultipleTimeSeries mts, LocalDate startMonth, LocalDate endMonth)
	{
		List<JTabbedPane> jTabbedPanes = new ArrayList<>();
		try
		{
			setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			DisplayWRIMSFrames displayWrimsFrames = new DisplayWRIMSFrames(plotConfigurationState, dts, mts, baseRun, scenarios,
					startMonth, endMonth);
			jTabbedPanes.addAll(displayWrimsFrames.showDisplayFrames());
		}
		catch(Throwable ex)
		{
			LOGGER.error("An error occurred while trying to display results", ex);
		}
		finally
		{
			setCursor(Cursor.getDefaultCursor());
		}
		return jTabbedPanes;

	}

	public interface PlotHandler extends Consumer<List<JTabbedPane>>
	{
		void openPlots(List<JTabbedPane> tabbedPanes);

		@Override
		default void accept(List<JTabbedPane> jTabbedPanes)
		{
			openPlots(jTabbedPanes);
		}
	}

}

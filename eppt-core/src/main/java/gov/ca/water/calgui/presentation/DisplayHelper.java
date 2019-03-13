/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.presentation.display.DefaultPlotHandler;
import org.apache.log4j.Logger;
import org.jfree.data.time.Month;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public class DisplayHelper
{
    private static final Logger LOGGER = Logger.getLogger(DisplayHelper.class.getName());

    private final  ExecutorService _executorService;
    private final Component _component;
    private static PlotHandler _topComponentPlotHandler = new DefaultPlotHandler();

    public DisplayHelper(Component comp)
    {
        _component = comp;
        _executorService = Executors.newSingleThreadExecutor(DisplayHelper::newThread);
    }

    public static void installPlotHandler(PlotHandler plotHandler)
    {
        _topComponentPlotHandler = plotHandler;
    }
    private static Thread newThread(Runnable r)
    {
        return new Thread(r, "DisplayHelperThread");
    }

    public void showDisplayFrames(String displayGroup, List<RBListItemBO> scenarios, Month startMonth,
                                  Month endMonth)
    {
            displayFramesOnBackground(displayGroup,scenarios,startMonth,endMonth);
    }
    public void showDisplayFramesWRIMS(String displayGroup, List<RBListItemBO> lstScenarios, DerivedTimeSeries dts,
                                       MultipleTimeSeries mts, Month startMonth, Month endMonth)
    {
        displayFramesWRIMSOnBackground(displayGroup,lstScenarios,dts,mts,startMonth,endMonth);
    }


    private void displayFramesOnBackground(String displayGroup, List<RBListItemBO> scenarios, Month startMonth,
                                           Month endMonth)
    {
        CompletableFuture.supplyAsync(() -> getTabbedPanes(displayGroup, scenarios, startMonth, endMonth), _executorService)
        .thenAcceptAsync(_topComponentPlotHandler::openPlots, SwingUtilities::invokeLater);


    }

    private List<JTabbedPane> getTabbedPanes(String displayGroup, List<RBListItemBO> scenarios, Month startMonth, Month endMonth)
    {
        List<JTabbedPane> jTabbedPanes = new ArrayList<>();
        try
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            jTabbedPanes  = DisplayFrame.showDisplayFrames(displayGroup, scenarios, startMonth, endMonth);
        }
        catch(RuntimeException ex)
        {
            LOGGER.error("An error occurred while trying to display the ");
        }
        finally
        {

            setCursor(Cursor.getDefaultCursor());
        }
        return jTabbedPanes;
    }

    private void setCursor(Cursor cursor)
    {
        SwingUtilities.invokeLater(()-> _component.setCursor(cursor));
    }

//    private void displayGlassPane()
//    {
//        JPanel glassPane = (JPanel)SwingUtilities.getRootPane(_component).getGlassPane();
//        SwingUtilities.invokeLater(()->glassPane.setVisible(true));
//
//    }

    private void displayFramesWRIMSOnBackground(String displayGroup, List<RBListItemBO> lstScenarios, DerivedTimeSeries dts,
                                                MultipleTimeSeries mts, Month startMonth, Month endMonth)
    {
        CompletableFuture.supplyAsync(() -> getTabbedPanesWRIMS(displayGroup, lstScenarios, dts, mts, startMonth, endMonth), _executorService)
                .thenAcceptAsync(_topComponentPlotHandler::openPlots, SwingUtilities::invokeLater);


    }

    private List<JTabbedPane> getTabbedPanesWRIMS(String displayGroup, List<RBListItemBO> lstScenarios, DerivedTimeSeries dts, MultipleTimeSeries mts, Month startMonth, Month endMonth)
    {
        List<JTabbedPane> jTabbedPanes = new ArrayList<>();
        try
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
            jTabbedPanes = DisplayFrame.showDisplayFramesWRIMS(displayGroup, lstScenarios, dts, mts, startMonth, endMonth);
        }
        catch(RuntimeException ex)
        {

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

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui;

import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.presentation.DisplayHelper;
import org.openide.windows.Mode;
import org.openide.windows.WindowManager;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-06-2019
 */
public class TopComponentPlotHandler implements DisplayHelper.PlotHandler
{
	@Override
	public void openPlots(List<JTabbedPane> tabbedPanes)
	{
		String resultsOutputLocation = EpptPreferences.getResultsOutputLocation();
		Mode output = WindowManager.getDefault().findMode(resultsOutputLocation);
		for(JTabbedPane tabbedpane : tabbedPanes)
		{
			PlotTopComponent topComponent = new PlotTopComponent(tabbedpane);
			output.dockInto(topComponent);
			topComponent.open();
			topComponent.requestActive();
			WindowManager.getDefault().setTopComponentFloating(topComponent, true);
		}
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui;

import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.presentation.DisplayFrame;
import org.openide.windows.Mode;
import org.openide.windows.WindowManager;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-06-2019
 */
public class TopComponentPlotHandler implements DisplayFrame.PlotHandler
{
	@Override
	public void openPlots(List<JTabbedPane> tabbedPanes)
	{

		Mode output = WindowManager.getDefault().findMode("output");
		for(JTabbedPane tabbedpane : tabbedPanes)
		{
			PlotTopComponent topComponent = new PlotTopComponent(tabbedpane);
			output.dockInto(topComponent);
			topComponent.open();
			topComponent.requestActive();
			//			if(!(doTimeSeries || doExceedance || doMonthlyTable || doSummaryTable))
			//			{
			//				if(dssGrabber.getMissingList() == null)
			//				{
			//					container.add(new JLabel("Nothing to show!"));
			//				}
			//			}
			//			else
			//			{
			//			}
		}
	}
}

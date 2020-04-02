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
		PlotTopComponent firstTc = null;
		for(JTabbedPane tabbedpane : tabbedPanes)
		{
			PlotTopComponent topComponent = new PlotTopComponent(tabbedpane);
			output.dockInto(topComponent);
			topComponent.open();
			if(firstTc == null)
			{
				topComponent.requestActive();
				firstTc = topComponent;
			}
		}
	}
}

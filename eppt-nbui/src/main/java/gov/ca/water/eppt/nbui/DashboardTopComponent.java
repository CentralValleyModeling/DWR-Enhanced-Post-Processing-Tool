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

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.quickresults.ui.customresults.CustomResultsListener;
import gov.ca.water.quickresults.ui.customresults.CustomResultsPanel;
import gov.ca.water.quickresults.ui.dashboard.DashboardPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "DashboardTopComponent",
		iconBase = "gov/ca/water/eppt/nbui/CustomResults.png"
)
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 3333)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.DashboardTopComponent")
@ActionReferences({
		@ActionReference(path = "Menu/Window", position = 3333),
		@ActionReference(path = "Toolbars/Window", position = 3333)
})
@TopComponent.OpenActionRegistration(
		displayName = "Custom Results",
		preferredID = "DashboardTopComponent"
)
@Messages(
		{
				"CTL_DashboardAction=Dashboard",
				"CTL_DashboardTopComponent=Dashboard Window",
				"HINT_DashboardTopComponent=This is a Dashboard window"
		})
public final class DashboardTopComponent extends EpptTopComponent
{


	public DashboardTopComponent()
	{
		setName("Dashboard");
		DashboardPanel customResultsPanel = new DashboardPanel();
		JScrollPane scrollPane = new JScrollPane(customResultsPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

	@Override
	public String getJavaHelpId()
	{
		return "Dashboard";
	}
}

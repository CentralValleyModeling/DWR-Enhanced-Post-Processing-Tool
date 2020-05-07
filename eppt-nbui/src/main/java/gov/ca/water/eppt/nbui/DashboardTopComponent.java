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
import java.awt.Color;
import java.util.Collection;
import javax.swing.*;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.eppt.nbui.actions.ProjectConfigurationSavable;
import gov.ca.water.trendreporting.TrendReportPanel;
import org.netbeans.api.actions.Savable;
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
		iconBase = "gov/ca/water/eppt/nbui/dashboard.png"
)
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 2000)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.DashboardTopComponent")
@ActionReferences({
		@ActionReference(path = "Menu/Window", position = 2000),
		@ActionReference(path = "Toolbars/Window", position = 2000)
})
@TopComponent.OpenActionRegistration(
		displayName = "Trend Reporting",
		preferredID = "DashboardTopComponent"
)
@Messages(
		{
				"CTL_DashboardAction=Trend Reporting",
				"CTL_DashboardTopComponent=Trend Reporting Window",
				"HINT_DashboardTopComponent=This is a Trend Reporting window"
		})
public final class DashboardTopComponent extends EpptTopComponent
{

	public DashboardTopComponent()
	{
		setName("Trend Reporting");
		EpptConfigurationController epptConfigurationController = EpptControllerProvider.getEpptConfigurationController();
		TrendReportPanel trendReportPanel = new TrendReportPanel(epptConfigurationController);
		epptConfigurationController.modifiedProperty().addListener((e,o,n)->
		{
			if(EpptPreferences.getAutoRefreshTrendReport())
			{
				trendReportPanel.inputsChanged();
			}
		});
		JScrollPane scrollPane = new JScrollPane();
		scrollPane.setViewportView(trendReportPanel);
		setLayout(new BorderLayout(15,15));
		add(scrollPane, BorderLayout.CENTER);
		setBackground(Color.WHITE);
	}

	@Override
	public String getJavaHelpId()
	{
		return "Trend Reporting";
	}

}

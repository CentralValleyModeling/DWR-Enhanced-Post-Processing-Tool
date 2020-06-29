/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
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

import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.quickresults.ui.global.PlotConfigurationPane;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "PlotConfigurationTopComponent",
		iconBase = "gov/ca/water/eppt/nbui/globalControl.png"
)
@TopComponent.Registration(mode = "explorer", openAtStartup = true, position = 1112)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.PlotConfigurationTopComponent")
@ActionReferences({
		@ActionReference(path = "Menu/Window", position = 1112),
		@ActionReference(path = "Toolbars/Window", position = 1112)
})
@TopComponent.OpenActionRegistration(
		displayName = "Global Controls",
		preferredID = "PlotConfigurationTopComponent"
)
@Messages(
		{
				"CTL_PlotConfigurationAction=Project Configuration",
				"CTL_PlotConfigurationTopComponent=Project Configuration Window",
				"HINT_PlotConfigurationTopComponent=This is the Project Configuration window"
		})
public final class PlotConfigurationTopComponent extends EpptTopComponent
{
	private static final String TOP_COMPONENT_NAME = "Global Controls";
	private PlotConfigurationPane _epptConfigurationPane;

	public PlotConfigurationTopComponent()
	{
		setName(TOP_COMPONENT_NAME);
		initComponents();
	}

	@Override
	public String getJavaHelpId()
	{
		return "3.1_ProjectConfiguration.htm";
	}

	@Override
	protected void componentDeactivated()
	{
		super.componentDeactivated();
		if(_epptConfigurationPane != null)
		{
			_epptConfigurationPane.commitEdits();
		}
	}

	private void initComponents()
	{
		EpptConfigurationController epptConfigurationController = EpptControllerProvider.getEpptConfigurationController();
		JFXPanel jfxPanel = new JFXPanel();
		Platform.runLater(() ->
		{
			_epptConfigurationPane = new PlotConfigurationPane(epptConfigurationController);
			_epptConfigurationPane.setPrefHeight(900);
			EpptControllerProvider.addListener(()->Platform.runLater(_epptConfigurationPane::reloadProject));
			jfxPanel.setScene(new Scene(_epptConfigurationPane));
			_epptConfigurationPane.reloadProject();
		});
		JScrollPane scrollPane = new JScrollPane(jfxPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}
}

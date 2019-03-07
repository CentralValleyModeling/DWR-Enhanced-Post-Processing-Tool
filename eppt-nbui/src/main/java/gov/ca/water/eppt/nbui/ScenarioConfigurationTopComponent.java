/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationListener;
import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "ScenarioConfigurationTopComponent",
		//iconBase="SET/PATH/TO/ICON/HERE",
		persistenceType = TopComponent.PERSISTENCE_ALWAYS
)
@TopComponent.Registration(mode = "explorer", openAtStartup = true, position = 1111)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.ScenarioConfigurationTopComponent")
@ActionReference(path = "Menu/Window", position = 1111)
@TopComponent.OpenActionRegistration(
		displayName = "Scenario Configuration",
		preferredID = "ScenarioConfigurationTopComponent"
)
@Messages(
		{
				"CTL_ScenarioConfigurationAction=Scenario Configuration",
				"CTL_ScenarioConfigurationTopComponent=Scenario Configuration Window",
				"HINT_ScenarioConfigurationTopComponent=This is the Scenario Configuration window"
		})
public final class ScenarioConfigurationTopComponent extends TopComponent
{

	public ScenarioConfigurationTopComponent()
	{
		setName("Scenario Configuration");
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
		ScenarioConfigurationListener scenarioConfigurationListener = new ScenarioConfigurationListener(
				scenarioConfigurationPanel);
		scenarioConfigurationPanel.setActionListener(scenarioConfigurationListener);
		JScrollPane scrollPane = new JScrollPane(scenarioConfigurationPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.quickresults.ui.ScenarioConfigurationPanel;
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
		displayName = "#CTL_ScenarioConfigurationAction",
		preferredID = "ScenarioConfigurationTopComponent"
)
@Messages(
		{
				"CTL_ScenarioConfigurationAction=ScenarioConfiguration",
				"CTL_ScenarioConfigurationTopComponent=ScenarioConfiguration Window",
				"HINT_ScenarioConfigurationTopComponent=This is a ScenarioConfiguration window"
		})
public final class ScenarioConfigurationTopComponent extends TopComponent
{

	public ScenarioConfigurationTopComponent()
	{
		setName("Scenario Configuration");
		ScenarioConfigurationPanel scenarioConfigurationPanel = new ScenarioConfigurationPanel();
		JScrollPane scrollPane = new JScrollPane(scenarioConfigurationPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

}

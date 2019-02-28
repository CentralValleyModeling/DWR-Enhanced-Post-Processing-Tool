/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.quickresults.ui.CustomResultsPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "CustomResultsTopComponent",
		//iconBase="SET/PATH/TO/ICON/HERE",
		persistenceType = TopComponent.PERSISTENCE_ALWAYS
)
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 3333)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.CustomResultsTopComponent")
@ActionReference(path = "Menu/Window", position = 3333)
@TopComponent.OpenActionRegistration(
		displayName = "#CTL_CustomResultsAction",
		preferredID = "CustomResultsTopComponent"
)
@Messages(
		{
				"CTL_CustomResultsAction=CustomResults",
				"CTL_CustomResultsTopComponent=CustomResults Window",
				"HINT_CustomResultsTopComponent=This is a CustomResults window"
		})
public final class CustomResultsTopComponent extends TopComponent
{

	public CustomResultsTopComponent()
	{
		setName("Custom Results");
		CustomResultsPanel customResultsPanel = new CustomResultsPanel(
				(JFrame) WindowManager.getDefault().getMainWindow());
		JScrollPane scrollPane = new JScrollPane(customResultsPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}
}

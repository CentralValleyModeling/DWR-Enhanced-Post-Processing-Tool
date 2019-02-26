/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.quickresults.ui.QuickResultsPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "QuickResultsTopComponent",
		//iconBase="SET/PATH/TO/ICON/HERE",
		persistenceType = TopComponent.PERSISTENCE_ALWAYS
)
@TopComponent.Registration(mode = "editor", openAtStartup = true)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.QuickResultsTopComponent")
@ActionReference(path = "Menu/Window" /*
 * , position = 333
 */)
@TopComponent.OpenActionRegistration(
		displayName = "#CTL_QuickResultsAction",
		preferredID = "QuickResultsTopComponent"
)
@Messages(
		{
				"CTL_QuickResultsAction=QuickResults",
				"CTL_QuickResultsTopComponent=QuickResults Window",
				"HINT_QuickResultsTopComponent=This is a QuickResults window"
		})
public final class QuickResultsTopComponent extends TopComponent
{

	public QuickResultsTopComponent()
	{
		setName("Quick Results");
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel();
		JScrollPane scrollPane = new JScrollPane(quickResultsPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

}

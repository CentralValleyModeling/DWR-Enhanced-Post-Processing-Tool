/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.quickresults.ui.quickresults.QuickResultsListener;
import gov.ca.water.quickresults.ui.quickresults.QuickResultsPanel;
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
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 2222)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.QuickResultsTopComponent")
@ActionReference(path = "Menu/Window", position = 2222)
@TopComponent.OpenActionRegistration(
		displayName = "Quick Results",
		preferredID = "QuickResultsTopComponent"
)
@Messages(
		{
				"CTL_QuickResultsAction=Quick Results",
				"CTL_QuickResultsTopComponent=Quick Results Window",
				"HINT_QuickResultsTopComponent=This is the Quick Results window"
		})
public final class QuickResultsTopComponent extends EpptTopComponent
{

	private final QuickResultsPanel _quickResultsPanel;

	public QuickResultsTopComponent()
	{
		setName("Quick Results");
		_quickResultsPanel = new QuickResultsPanel();
		DisplayHelper.installPlotHandler(new TopComponentPlotHandler());
		QuickResultsListener quickResultsListener = new QuickResultsListener(_quickResultsPanel);
		_quickResultsPanel.setActionListener(quickResultsListener);
		JScrollPane scrollPane = new JScrollPane(_quickResultsPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

	@Override
	public String getJavaHelpId()
	{
		return _quickResultsPanel.getJavaHelpId();
	}
}

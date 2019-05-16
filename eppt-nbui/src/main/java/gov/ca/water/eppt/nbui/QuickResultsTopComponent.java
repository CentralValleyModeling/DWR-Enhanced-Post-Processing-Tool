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

import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.quickresults.ui.quickresults.QuickResultsListener;
import gov.ca.water.quickresults.ui.quickresults.QuickResultsPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "QuickResultsTopComponent",
		iconBase = "gov/ca/water/eppt/nbui/QuickResults.png"
)
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 2222)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.QuickResultsTopComponent")
@ActionReferences({
		@ActionReference(path = "Menu/Window", position = 2222),
		@ActionReference(path = "Toolbars/Window", position = 2222)
})

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

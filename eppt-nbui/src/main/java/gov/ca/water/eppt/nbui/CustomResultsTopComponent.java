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
import gov.ca.water.calgui.project.EpptConfigurationController;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "CustomResultsTopComponent",
		iconBase = "gov/ca/water/eppt/nbui/CustomResults.png"
)
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 4000)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.CustomResultsTopComponent")
@ActionReferences({
		@ActionReference(path = "Menu/Window", position = 4000),
		@ActionReference(path = "Toolbars/Window", position = 4000)
})
@TopComponent.OpenActionRegistration(
		displayName = "Custom Results",
		preferredID = "CustomResultsTopComponent"
)
@Messages(
		{
				"CTL_CustomResultsAction=CustomResults",
				"CTL_CustomResultsTopComponent=CustomResults Window",
				"HINT_CustomResultsTopComponent=This is a CustomResults window"
		})
public final class CustomResultsTopComponent extends EpptTopComponent
{

	private final CustomResultsPanel _customResultsPanel;

	public CustomResultsTopComponent()
	{
		setName("Custom Results");
		EpptConfigurationController epptConfigurationController = EpptControllerProvider.getEpptConfigurationController();
		_customResultsPanel = new CustomResultsPanel(epptConfigurationController);
		CustomResultsListener customResultsListener = new CustomResultsListener(_customResultsPanel);
		_customResultsPanel.setActionListener(customResultsListener);
		JScrollPane scrollPane = new JScrollPane(_customResultsPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

	@Override
	public String getJavaHelpId()
	{
		return _customResultsPanel.getJavaHelpId();
	}
}

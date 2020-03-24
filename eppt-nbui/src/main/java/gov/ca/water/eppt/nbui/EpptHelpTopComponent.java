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
import java.net.MalformedURLException;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.help.HelpSet;
import javax.help.HelpSetException;
import javax.help.JHelp;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.HelpPanel;
import gov.ca.water.quickresults.ui.customresults.CustomResultsPanel;
import org.apache.log4j.Logger;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "EpptHelpTopComponent",
		iconBase = "gov/ca/water/eppt/nbui/questionmark18.gif"
)
@TopComponent.Registration(mode = "editor", openAtStartup = false)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.EpptHelpTopComponent")
@ActionReference(path = "Menu/Help", position = 0)
@TopComponent.OpenActionRegistration(
		displayName = "EPPT Help",
		preferredID = "EpptHelpTopComponent"
)
public final class EpptHelpTopComponent extends TopComponent
{
	private final HelpPanel _helpViewHtml5;

	public EpptHelpTopComponent() throws EpptInitializationException
	{
		setName("EPPT Help");
		_helpViewHtml5 = new HelpPanel();
		setLayout(new BorderLayout());
		add(this._helpViewHtml5, BorderLayout.CENTER);
	}

	private void selectActivatedHelp()
	{
		TopComponent activated = TopComponent.getRegistry().getActivated();
		if(activated instanceof EpptTopComponent)
		{
			String javaHelpId = ((EpptTopComponent) activated).getJavaHelpId();
			selectCurrentHelperId(javaHelpId);
		}
	}

	void selectCurrentHelperId(String page)
	{
		_helpViewHtml5.loadHelp(page);
	}

	@Override
	protected void componentOpened()
	{
		super.componentOpened();
		selectActivatedHelp();
		SwingUtilities.invokeLater(() -> WindowManager.getDefault().setTopComponentFloating(this, true));
	}
}

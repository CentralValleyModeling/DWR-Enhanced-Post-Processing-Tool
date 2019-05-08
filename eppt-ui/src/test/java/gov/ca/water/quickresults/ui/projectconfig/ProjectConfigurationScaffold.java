/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.BorderLayout;

import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.EpptScaffold;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class ProjectConfigurationScaffold extends EpptScaffold
{


	public static void main(String[] args) throws EpptInitializationException
	{
		new ProjectConfigurationScaffold().initScaffold();
	}

	@Override
	protected EpptPanel buildEpptPanel()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		JButton reset = new JButton("Reset");
		reset.addActionListener(e-> {
			try
			{
				projectConfigurationPanel.resetProjectConfiguration();
				projectConfigurationPanel.add(reset, BorderLayout.SOUTH);
			}
			catch(Exception e1)
			{
				e1.printStackTrace();
			}
		});
//		projectConfigurationPanel.add(reset, BorderLayout.SOUTH);
		ProjectConfigurationListener projectConfigurationListener = new ProjectConfigurationListener(
				projectConfigurationPanel);
		projectConfigurationPanel.setActionListener(projectConfigurationListener);
		return projectConfigurationPanel;
	}
}

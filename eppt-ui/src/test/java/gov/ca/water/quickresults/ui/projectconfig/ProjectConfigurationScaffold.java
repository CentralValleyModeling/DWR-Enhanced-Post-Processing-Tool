/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.projectconfig;

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
		ProjectConfigurationListener projectConfigurationListener = new ProjectConfigurationListener(
				projectConfigurationPanel);
		projectConfigurationPanel.getSwingEngine().setActionListener(projectConfigurationPanel,
				projectConfigurationListener);
		return projectConfigurationPanel;
	}
}

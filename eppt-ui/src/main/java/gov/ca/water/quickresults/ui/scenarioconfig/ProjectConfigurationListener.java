/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.scenarioconfig;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import gov.ca.water.calgui.bo.FileDialogBO;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.quickresults.ui.EpptPanel;
import org.apache.log4j.Logger;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class ProjectConfigurationListener implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(ProjectConfigurationListener.class.getName());
	private final ProjectConfigurationPanel _projectConfigurationPanel;
	private final FileDialogBO _addScnearioFileDialogBO;

	public ProjectConfigurationListener(ProjectConfigurationPanel projectConfigurationPanel)
	{
		_projectConfigurationPanel = projectConfigurationPanel;

		DefaultListModel<RBListItemBO> lstScenarios = _projectConfigurationPanel.getLmScenNames();
		_addScnearioFileDialogBO = new FileDialogBO(lstScenarios, true,
				projectConfigurationPanel);
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		switch(e.getActionCommand())
		{
			case "Rep_ClearMonths":
				setQRMonthCheckBoxesSelected(false);
				break;
			case "Rep_All":
				setQRMonthCheckBoxesSelected(true);
				break;
			case "btnAddScenario":
				launchFileDialogToAddScenarios(e);
				break;
			case "btnDelScenario":
				launchFileDialogToAddScenarios(e);
				break;
			case "btnClearScenario":
				launchFileDialogToAddScenarios(e);
				break;
			default:
		}
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		projectConfigurationPanel.getScenarioList().repaint();
		projectConfigurationPanel.setModified(true);
	}

	private void launchFileDialogToAddScenarios(ActionEvent e)
	{
		_addScnearioFileDialogBO.actionPerformed(e);
	}

	/**
	 * Selects/deselects all monthly checkboxes on Quick Result control panel
	 *
	 * @param b
	 */
	private void setQRMonthCheckBoxesSelected(boolean b)
	{
		JPanel controls2 = _projectConfigurationPanel.getControls2();
		EpptPanel.setCheckboxesSelectedRecusive(b, controls2);
	}

}

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

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.quickresults.ui.EpptPanel;


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
	private final ScenarioChooserBO _addScnearioFileDialogBO;

	public ProjectConfigurationListener(ProjectConfigurationPanel projectConfigurationPanel)
	{
		_projectConfigurationPanel = projectConfigurationPanel;

		DefaultListModel<RBListItemBO> lstScenarios = _projectConfigurationPanel.getLmScenNames();
		_addScnearioFileDialogBO = new ScenarioChooserBO(lstScenarios, projectConfigurationPanel);
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
				ProjectConfigurationPanel.getProjectConfigurationPanel().deleteScenario();
				break;
			case "btnClearScenario":
				ProjectConfigurationPanel.getProjectConfigurationPanel().clearAllScenarios();
				break;
			default:
		}
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		projectConfigurationPanel.getScenarioList().repaint();
		projectConfigurationPanel.setModified(true);
	}

	private void launchFileDialogToAddScenarios(ActionEvent e)
	{
		_addScnearioFileDialogBO.createScenario();
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

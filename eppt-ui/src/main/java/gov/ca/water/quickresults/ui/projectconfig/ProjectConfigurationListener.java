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

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.projectconfig.scenarioconfig.ScenarioRunEditor;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class ProjectConfigurationListener implements ActionListener
{
	private final ProjectConfigurationPanel _projectConfigurationPanel;

	public ProjectConfigurationListener(ProjectConfigurationPanel projectConfigurationPanel)
	{
		_projectConfigurationPanel = projectConfigurationPanel;
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
				launchFileDialogToAddScenario(e);
				break;
			case "btnEditScenario":
				launchFileDialogToEditScenario(e);
				break;
			case "btnDelScenario":
				_projectConfigurationPanel.deleteScenario();
				break;
			case "btnClearScenario":
				_projectConfigurationPanel.clearAllScenarios();
				break;
			case "moveUp":
				_projectConfigurationPanel.moveSelectedScenarioUp();
				break;
			case "moveDown":
				_projectConfigurationPanel.moveSelectedScenarioDown();
				break;
			default:
		}
	}

	private void launchFileDialogToAddScenario(ActionEvent e)
	{
		ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(
				(Frame) SwingUtilities.windowForComponent(_projectConfigurationPanel));
		scenarioRunEditor.setVisible(true);
		scenarioRunEditor.dispose();
		EpptScenarioRun scenarioRun = scenarioRunEditor.createRun();
		if(scenarioRun != null)
		{
			_projectConfigurationPanel.addScenario(scenarioRun);
		}
	}

	private void launchFileDialogToEditScenario(ActionEvent e)
	{
		EpptScenarioRun oldScenarioRun = _projectConfigurationPanel.getSelectedScenario();
		if(oldScenarioRun != null)
		{
			ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(
					(Frame) SwingUtilities.windowForComponent(_projectConfigurationPanel));
			scenarioRunEditor.fillPanel(oldScenarioRun);
			scenarioRunEditor.setVisible(true);
			EpptScenarioRun newScenarioRun = scenarioRunEditor.createRun();
			scenarioRunEditor.setVisible(false);
			scenarioRunEditor.dispose();

			if(newScenarioRun != null)
			{
				_projectConfigurationPanel.replaceScenario(oldScenarioRun, newScenarioRun);
			}
		}
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
		_projectConfigurationPanel.setModified(true);
	}

}

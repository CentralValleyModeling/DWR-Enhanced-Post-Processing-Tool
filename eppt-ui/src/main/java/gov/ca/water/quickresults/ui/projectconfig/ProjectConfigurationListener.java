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

import java.awt.Desktop;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.nio.file.Path;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.projectconfig.scenarioconfig.ScenarioRunEditor;

import hec.gui.NameDialog;

import static java.util.stream.Collectors.toList;


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
			case "btnCopyScenario":
				launchFileDialogToCopyScenario(e);
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
			case "openProject":
				openCurrentProject();
				break;
			default:
		}
	}

	private void openCurrentProject()
	{
		Path lastProjectConfiguration = EpptPreferences.getLastProjectConfiguration();
		if(!lastProjectConfiguration.toString().isEmpty())
		{
			Path projectFolder = lastProjectConfiguration.getParent();
			try
			{
				Desktop.getDesktop().open(projectFolder.toFile());
			}
			catch(IOException e)
			{
				LOGGER.log(Level.SEVERE, "Error opening project folder: " + projectFolder, e);
			}
		}
	}

	private void launchFileDialogToAddScenario(ActionEvent e)
	{
		ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(
				(Frame) SwingUtilities.windowForComponent(_projectConfigurationPanel));
		scenarioRunEditor.setVisible(true);
		scenarioRunEditor.dispose();
		EpptScenarioRun scenarioRun = scenarioRunEditor.createRun();
		if(scenarioRun != null && !scenarioRunEditor.isCanceled())
		{
			_projectConfigurationPanel.addScenario(scenarioRun);
		}
	}

	private void launchFileDialogToCopyScenario(ActionEvent e)
	{
		EpptScenarioRun oldScenarioRun = _projectConfigurationPanel.getSelectedScenario();
		if(oldScenarioRun != null)
		{
			NameDialog nameDialog = new NameDialog((Frame) SwingUtilities.windowForComponent(_projectConfigurationPanel), true);
			nameDialog.setExistingNames(ProjectConfigurationPanel.getProjectConfigurationPanel()
																 .getAllEpptScenarioRuns()
																 .stream()
																 .map(EpptScenarioRun::getName)
																 .collect(toList()));
			nameDialog.setTitle("Copy Scenario Run");
			nameDialog.setName(oldScenarioRun.getName() + " (Copy)");
			nameDialog.setDescription(oldScenarioRun.getDescription());
			nameDialog.setVisible(true);
			if(!nameDialog.isCanceled())
			{
				String name = nameDialog.getName();
				String description = nameDialog.getDescription();
				EpptScenarioRun newScenarioRun = new EpptScenarioRun(name, description, oldScenarioRun);
				_projectConfigurationPanel.addScenario(newScenarioRun);
				_projectConfigurationPanel.updateRadioState();
			}
			nameDialog.dispose();
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

			if(newScenarioRun != null && !scenarioRunEditor.isCanceled())
			{
				_projectConfigurationPanel.replaceScenario(oldScenarioRun, newScenarioRun);
				_projectConfigurationPanel.updateRadioState();
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

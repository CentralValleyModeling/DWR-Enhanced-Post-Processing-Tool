/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui.actions;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.WindowManager;

@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.LoadScenarioConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/HecDssVue.png",
		displayName = "Load Scenario Configuration"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 11, separatorAfter = 50)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 333)
		})
@Messages("CTL_LoadScenarioConfiguration=Load Scenario Configuration")
public final class LoadScenarioConfiguration implements ActionListener
{

	private static final Logger LOGGER = Logger.getLogger(LoadScenarioConfiguration.class.getName());

	@Override
	public void actionPerformed(ActionEvent e)
	{
		JFileChooser fileChooser = new JFileChooser(EpptPreferences.getProjectsPath().toFile());
		fileChooser.setFileFilter(new SimpleFileFilter(Constant.EPPT_EXT, "EPPT Project File"));
		fileChooser.setMultiSelectionEnabled(false);
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		fileChooser.setDialogType(JFileChooser.OPEN_DIALOG);
		fileChooser.showOpenDialog(WindowManager.getDefault().getMainWindow());
		File selectedFile = fileChooser.getSelectedFile();
		if(selectedFile != null && selectedFile.exists())
		{
			Path selectedPath = selectedFile.toPath();
			try
			{
				ScenarioConfigurationPanel.getScenarioConfigurationPanel().loadScenarioConfiguration(selectedPath);
			}
			catch(IOException ex)
			{
				LOGGER.log(Level.SEVERE, "Error saving Scenario Configuration to: " + selectedPath, ex);
			}
		}
	}
}

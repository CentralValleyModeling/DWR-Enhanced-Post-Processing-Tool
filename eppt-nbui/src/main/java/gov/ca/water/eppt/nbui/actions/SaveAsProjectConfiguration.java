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
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.bo.SimpleFileFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.eppt.nbui.Installer;
import gov.ca.water.quickresults.ui.scenarioconfig.ProjectConfigurationPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.Lookup;
import org.openide.util.NbBundle.Messages;
import org.openide.util.Utilities;
import org.openide.windows.WindowManager;

@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.SaveAsProjectConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/save.png",
		displayName = "Save As..."
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 2)
		})
@Messages("CTL_SaveAsScenarioConfiguration=Save Project Configuration As...")
public final class SaveAsProjectConfiguration implements ActionListener
{

	private static final Logger LOGGER = Logger.getLogger(SaveAsProjectConfiguration.class.getName());
	private static final PathMatcher EPPT_EXTENSION_MATCHER = FileSystems.getDefault().getPathMatcher(
			"glob:*." + Constant.EPPT_EXT);
	private Lookup.Result<ProjectConfigurationSavable> _lkpInfo;

	public SaveAsProjectConfiguration()
	{
		this(Utilities.actionsGlobalContext());
	}

	private SaveAsProjectConfiguration(Lookup context)
	{
		_lkpInfo = context.lookupResult(ProjectConfigurationSavable.class);
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		try
		{
			saveAs();
		}
		catch(IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Error saving Project Configuration", ex);
		}
	}

	public void saveAs() throws IOException
	{
		JFileChooser fileChooser = new JFileChooser(EpptPreferences.getProjectsPath().toFile());
		fileChooser.setFileFilter(new SimpleFileFilter(Constant.EPPT_EXT, "EPPT Project File"));
		fileChooser.setMultiSelectionEnabled(false);
		fileChooser.setDialogType(JFileChooser.SAVE_DIALOG);
		fileChooser.showSaveDialog(WindowManager.getDefault().getMainWindow());
		File selectedFile = fileChooser.getSelectedFile();
		if(selectedFile != null)
		{
			Path selectedPath = selectedFile.toPath();
			if(!EPPT_EXTENSION_MATCHER.matches(selectedPath.getFileName()))
			{
				selectedPath = Paths.get(selectedPath + "." + Constant.EPPT_EXT);
			}
			ProjectConfigurationPanel.getProjectConfigurationPanel().saveConfigurationToPath(selectedPath);
			WindowManager.getDefault().getMainWindow().setTitle(
					Installer.MAIN_FRAME_NAME + " - " + ProjectConfigurationPanel.getProjectConfigurationPanel().getProjectName());
			ProjectConfigurationPanel.getProjectConfigurationPanel().setModified(false);
		}
		Collection<? extends ProjectConfigurationSavable> scenarioConfigurationSavables = _lkpInfo.allInstances();
		for(ProjectConfigurationSavable savable : scenarioConfigurationSavables)
		{
			savable.removeFromLookup();
		}
	}
}

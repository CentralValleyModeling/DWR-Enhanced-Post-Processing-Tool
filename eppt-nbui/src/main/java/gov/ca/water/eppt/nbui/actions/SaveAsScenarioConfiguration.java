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
import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationPanel;
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
		id = "gov.ca.water.eppt.nbui.actions.SaveAsScenarioConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/save.png",
		displayName = "Save As..."
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 2)
		})
@Messages("CTL_SaveAsScenarioConfiguration=Save Scenario Configuration As...")
public final class SaveAsScenarioConfiguration implements ActionListener
{

	private static final Logger LOGGER = Logger.getLogger(SaveAsScenarioConfiguration.class.getName());
	private static final PathMatcher EPPT_EXTENSION_MATCHER = FileSystems.getDefault().getPathMatcher(
			"glob:*." + Constant.EPPT_EXT);
	private Lookup.Result<ScenarioConfigurationSavable> _lkpInfo;

	public SaveAsScenarioConfiguration()
	{
		this(Utilities.actionsGlobalContext());
	}

	private SaveAsScenarioConfiguration(Lookup context)
	{
		_lkpInfo = context.lookupResult(ScenarioConfigurationSavable.class);
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
			LOGGER.log(Level.SEVERE, "Error saving Scenario Configuration", ex);
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
			ScenarioConfigurationPanel.getScenarioConfigurationPanel().saveConfigurationToPath(selectedPath);
			ScenarioConfigurationPanel.getScenarioConfigurationPanel().setModified(false);
		}
		Collection<? extends ScenarioConfigurationSavable> scenarioConfigurationSavables = _lkpInfo.allInstances();
		for(ScenarioConfigurationSavable savable : scenarioConfigurationSavables)
		{
			savable.removeFromLookup();
		}
	}
}

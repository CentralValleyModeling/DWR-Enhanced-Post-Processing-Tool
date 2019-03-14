/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui.actions;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
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
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.NbBundle.Messages;
import org.openide.util.Utilities;
import org.openide.util.actions.Presenter;
import org.openide.windows.WindowManager;

@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.OpenProjectConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/open.png",
		displayName = "Open..."
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 300, separatorAfter = 555)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 333, separatorAfter = 444)
		})
@Messages("CTL_LoadProjectConfiguration=Open...")
public final class OpenProjectConfiguration extends AbstractAction
		implements Presenter.Toolbar, Presenter.Menu, ContextAwareAction
{

	private static final Logger LOGGER = Logger.getLogger(OpenProjectConfiguration.class.getName());
	private Lookup.Result<ProjectConfigurationSavable> _lkpInfo;

	public OpenProjectConfiguration()
	{
		this(Utilities.actionsGlobalContext());
	}

	private OpenProjectConfiguration(Lookup context)
	{
		putValue(Action.NAME, "Open...");
		putValue(Action.SMALL_ICON, "gov/ca/water/eppt/nbui/actions/open.png");
		putValue(Action.LARGE_ICON_KEY, "gov/ca/water/eppt/nbui/actions/open24.png");
		_lkpInfo = context.lookupResult(ProjectConfigurationSavable.class);
	}

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
				ProjectConfigurationPanel.getProjectConfigurationPanel().loadProjectConfiguration(selectedPath);
				WindowManager.getDefault().getMainWindow().setTitle(
						Installer.MAIN_FRAME_NAME + " - " + ProjectConfigurationPanel.getProjectConfigurationPanel().getProjectName());
				Collection<? extends ProjectConfigurationSavable> scenarioConfigurationSavables = _lkpInfo.allInstances();
				for(ProjectConfigurationSavable savable : scenarioConfigurationSavables)
				{
					savable.removeFromLookup();
				}
			}
			catch(IOException ex)
			{
				LOGGER.log(Level.SEVERE, "Error saving Project Configuration to: " + selectedPath, ex);
			}
		}
	}

	@Override
	public Component getToolbarPresenter()
	{
		ImageIcon imageIcon = getSaveIcon("open24.png");
		JButton button = new JButton(imageIcon);
		button.setToolTipText("Open Project Configuration");
		button.addActionListener(this);
		return button;
	}

	@Override
	public JMenuItem getMenuPresenter()
	{
		ImageIcon imageIcon = getSaveIcon("open.png");
		JMenuItem jMenuItem = new JMenuItem("Open...", imageIcon);
		jMenuItem.addActionListener(this);
		return jMenuItem;
	}

	private ImageIcon getSaveIcon(String iconName)
	{
		ImageIcon imageIcon = new ImageIcon("");
		URL saveImg = Thread.currentThread().getContextClassLoader().getResource(
				"gov/ca/water/eppt/nbui/actions/" + iconName);
		if(saveImg != null)
		{
			imageIcon = new ImageIcon(saveImg);
		}
		return imageIcon;
	}

	@Override
	public Action createContextAwareInstance(Lookup context)
	{
		return new OpenProjectConfiguration(context);
	}
}

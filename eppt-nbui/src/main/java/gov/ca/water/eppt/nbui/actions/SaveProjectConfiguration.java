/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui.actions;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.eppt.nbui.Installer;
import gov.ca.water.quickresults.ui.scenarioconfig.ProjectConfigurationPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.awt.DropDownButtonFactory;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.NbBundle.Messages;
import org.openide.util.Utilities;
import org.openide.util.actions.Presenter;
import org.openide.windows.WindowManager;

@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.SaveProjectConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/HecDssVue.png",
		displayName = "Save"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 100)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 111)
		})
@Messages("CTL_SaveScenarioConfiguration=Save Project Configuration")
public final class SaveProjectConfiguration extends AbstractAction implements Presenter.Toolbar, Presenter.Menu,
																			  ContextAwareAction
{
	private static final Logger LOGGER = Logger.getLogger(SaveProjectConfiguration.class.getName());
	private Lookup.Result<ProjectConfigurationSavable> _lkpInfo;

	public SaveProjectConfiguration()
	{
		this(Utilities.actionsGlobalContext());
	}

	private SaveProjectConfiguration(Lookup context)
	{
		putValue(Action.NAME, "Save");
		_lkpInfo = context.lookupResult(ProjectConfigurationSavable.class);
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		LOGGER.log(Level.FINEST, "No-Opped method. Using toolbar presenter instead");
	}

	void saveCurrentConfiguration() throws IOException
	{
		Path lastScenarioConfiguration = EpptPreferences.getLastProjectConfiguration();
		if(lastScenarioConfiguration.toFile().exists())
		{
			ProjectConfigurationPanel.getProjectConfigurationPanel()
									 .saveConfigurationToPath(lastScenarioConfiguration);
		}
		else
		{
			new SaveAsProjectConfiguration().saveAs();
		}
		WindowManager.getDefault().getMainWindow().setTitle(
				Installer.MAIN_FRAME_NAME + " - " + ProjectConfigurationPanel.getProjectConfigurationPanel().getProjectName());
		Collection<? extends ProjectConfigurationSavable> scenarioConfigurationSavables = _lkpInfo.allInstances();
		for(ProjectConfigurationSavable savable : scenarioConfigurationSavables)
		{
			savable.removeFromLookup();
		}
	}


	@Override
	public Component getToolbarPresenter()
	{
		JPopupMenu menu = new JPopupMenu();
		JMenuItem jMenuItem = new JMenuItem("Save As...");
		jMenuItem.addActionListener(new SaveAsProjectConfiguration());
		menu.add(jMenuItem);
		ImageIcon imageIcon = getSaveIcon("save24.png");
		JToggleButton dropDownToggleButton = DropDownButtonFactory.createDropDownToggleButton(imageIcon, menu);
		dropDownToggleButton.addActionListener(e -> performSave());
		return dropDownToggleButton;
	}

	@Override
	public JMenuItem getMenuPresenter()
	{
		ImageIcon imageIcon = getSaveIcon("save.png");
		JMenuItem jMenuItem = new JMenuItem("Save", imageIcon);
		jMenuItem.addActionListener(e -> performSave());
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

	private void performSave()
	{
		try
		{
			saveCurrentConfiguration();
		}
		catch(IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Error saving current Project Configuration to:", ex);
		}
	}

	@Override
	public Action createContextAwareInstance(Lookup context)
	{
		return new SaveProjectConfiguration(context);
	}
}

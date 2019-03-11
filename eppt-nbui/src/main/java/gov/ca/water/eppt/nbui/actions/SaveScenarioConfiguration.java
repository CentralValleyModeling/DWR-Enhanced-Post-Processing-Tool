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
import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationPanel;
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

@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.SaveScenarioConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/HecDssVue.png",
		displayName = "Save Scenario Configuration"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 0)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 111)
		})
@Messages("CTL_SaveScenarioConfiguration=Save Scenario Configuration")
public final class SaveScenarioConfiguration extends AbstractAction implements Presenter.Toolbar, Presenter.Menu,
																			   ContextAwareAction
{
	private static final Logger LOGGER = Logger.getLogger(SaveScenarioConfiguration.class.getName());
	private Lookup.Result<ScenarioConfigurationSavable> _lkpInfo;

	public SaveScenarioConfiguration()
	{
		this(Utilities.actionsGlobalContext());
	}

	private SaveScenarioConfiguration(Lookup context)
	{
		putValue(Action.NAME, "Save Scenario Configuration");
		_lkpInfo = context.lookupResult(ScenarioConfigurationSavable.class);
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		LOGGER.log(Level.FINEST, "No-Opped method. Using toolbar presenter instead");
	}

	public void saveCurrentConfiguration() throws IOException
	{
		Path lastScenarioConfiguration = EpptPreferences.getLastScenarioConfiguration();
		if(lastScenarioConfiguration.toFile().exists())
		{
			ScenarioConfigurationPanel.getScenarioConfigurationPanel()
									  .saveConfigurationToPath(lastScenarioConfiguration);
		}
		else
		{
			new SaveAsScenarioConfiguration().saveAs();
		}
		Collection<? extends ScenarioConfigurationSavable> scenarioConfigurationSavables = _lkpInfo.allInstances();
		for(ScenarioConfigurationSavable savable : scenarioConfigurationSavables)
		{
			savable.removeFromLookup();
		}
	}


	@Override
	public Component getToolbarPresenter()
	{
		JPopupMenu menu = new JPopupMenu();
		JMenuItem jMenuItem = new JMenuItem("Save As...");
		jMenuItem.addActionListener(new SaveAsScenarioConfiguration());
		menu.add(jMenuItem);
		ImageIcon imageIcon = getSaveIcon();
		JToggleButton dropDownToggleButton = DropDownButtonFactory.createDropDownToggleButton(imageIcon, menu);
		dropDownToggleButton.addActionListener(e -> performSave());
		return dropDownToggleButton;
	}

	@Override
	public JMenuItem getMenuPresenter()
	{
		ImageIcon imageIcon = getSaveIcon();
		JMenuItem jMenuItem = new JMenuItem("Save Scenario Configuration", imageIcon);
		jMenuItem.addActionListener(e -> performSave());
		return jMenuItem;
	}

	private ImageIcon getSaveIcon()
	{
		ImageIcon imageIcon = new ImageIcon("");
		URL saveImg = Thread.currentThread().getContextClassLoader().getResource(
				"/gov/ca/water/eppt/nbui/actions/save.png");
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
			LOGGER.log(Level.SEVERE, "Error saving current Scenario Configuration to:", ex);
		}
	}

	@Override
	public Action createContextAwareInstance(Lookup context)
	{
		return new SaveScenarioConfiguration(context);
	}
}

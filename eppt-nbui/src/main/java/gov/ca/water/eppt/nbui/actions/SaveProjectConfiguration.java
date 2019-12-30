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
package gov.ca.water.eppt.nbui.actions;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.eppt.nbui.Installer;
import gov.ca.water.eppt.nbui.ProjectConfigurationTopComponent;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import org.netbeans.api.actions.Savable;
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
import org.openide.windows.Mode;
import org.openide.windows.WindowManager;

@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.SaveProjectConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/save.png",
		displayName = "Save"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 100)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 111)
		})
@Messages("CTL_SaveProjectConfiguration=Save Project Configuration")
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
		Path lastProjectConfiguration = EpptPreferences.getLastProjectConfiguration();
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		if(lastProjectConfiguration.toFile().exists())
		{
			projectConfigurationPanel.saveConfigurationToPath(lastProjectConfiguration,
					projectConfigurationPanel.getProjectName(), projectConfigurationPanel.getProjectDescription());
		}
		else
		{
			new NewProjectConfiguration().createNew();
		}
		clearSaveCookie();
	}

	static void clearSaveCookie()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		WindowManager.getDefault().getMainWindow().setTitle(
				Installer.MAIN_FRAME_NAME + " - " + projectConfigurationPanel.getProjectName());
		Collection<? extends ProjectConfigurationSavable> projectConfigurationSavables = Savable.REGISTRY.lookupAll(
				ProjectConfigurationSavable.class);
		if(projectConfigurationSavables.isEmpty())
		{
			WindowManager.getDefault().getModes()
						 .stream()
						 .map(Mode::getTopComponents)
						 .map(Arrays::asList)
						 .flatMap(Collection::stream)
						 .filter(t -> t instanceof ProjectConfigurationTopComponent)
						 .map(t -> (ProjectConfigurationTopComponent) t)
						 .forEach(ProjectConfigurationTopComponent::topComponentNameUnmodified);
		}
		else
		{
			for(ProjectConfigurationSavable savable : projectConfigurationSavables)
			{
				savable.removeFromLookup();
			}
		}
	}


	@Override
	public Component getToolbarPresenter()
	{
		ImageIcon imageIcon = getSaveIcon("save24.png");
		JPopupMenu menu = new JPopupMenu();
		menu.add(createSaveAsAction());
		JButton dropDownButton = DropDownButtonFactory.createDropDownButton(imageIcon, menu);
		dropDownButton.addActionListener(e -> performSave());
		return dropDownButton;
	}

	private JMenuItem createSaveAsAction()
	{
		ImageIcon imageIcon = getSaveIcon("save24.png");
		JMenuItem retval = new JMenuItem("Save As...", imageIcon);
		retval.addActionListener(e -> saveAs());
		return retval;
	}

	private void saveAs()
	{
		new SaveAsProjectConfiguration().actionPerformed(null);
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
		catch(IOException | RuntimeException ex)
		{
			LOGGER.log(Level.SEVERE, "Error saving current Project Configuration to:" + EpptPreferences.getLastProjectConfiguration(), ex);
		}
	}

	@Override
	public Action createContextAwareInstance(Lookup context)
	{
		return new SaveProjectConfiguration(context);
	}
}

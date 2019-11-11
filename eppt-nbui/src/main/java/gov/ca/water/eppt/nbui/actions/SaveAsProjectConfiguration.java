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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.eppt.nbui.Installer;
import gov.ca.water.eppt.nbui.ProjectConfigurationTopComponent;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import org.netbeans.api.actions.Savable;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.Mode;
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
				@ActionReference(path = "Menu/File", position = 200)
		})
@Messages("CTL_SaveAsProjectConfiguration=Save As...")
public final class SaveAsProjectConfiguration implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(SaveAsProjectConfiguration.class.getName());

	@Override
	public void actionPerformed(ActionEvent e)
	{
		try
		{
			saveAs();
		}
		catch(IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Error saving project configuration", ex);
		}
	}

	private void saveAs() throws IOException
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		new NewProjectConfiguration().createNew(false);
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
}

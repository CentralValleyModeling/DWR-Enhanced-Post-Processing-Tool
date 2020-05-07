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
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.eppt.nbui.EpptControllerProvider;
import gov.ca.water.eppt.nbui.Installer;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.eppt.nbui.projectconfig.ProjectConfigurationIO;
import org.netbeans.api.actions.Savable;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.Lookup;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;
import org.openide.windows.WindowManager;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-13-2019
 */
@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.NewProjectConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/new.png",
		displayName = "New..."
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 0)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 0)
		})
@NbBundle.Messages("CTL_NewProjectConfiguration=New...")
public class NewProjectConfiguration implements ActionListener
{

	private static final Logger LOGGER = Logger.getLogger(NewProjectConfiguration.class.getName());

	private static final PathMatcher EPPT_EXTENSION_MATCHER = FileSystems.getDefault().getPathMatcher(
			"glob:*." + Constant.EPPT_EXT);
	private Lookup.Result<ProjectConfigurationSavable> _lkpInfo;

	public NewProjectConfiguration()
	{
		this(Utilities.actionsGlobalContext());
	}

	private NewProjectConfiguration(Lookup context)
	{
		_lkpInfo = context.lookupResult(ProjectConfigurationSavable.class);
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		try
		{
			createNew();
		}
		catch(IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Error saving Project Configuration", ex);
		}
	}

	void createNew() throws IOException
	{
		createNew(new EpptConfigurationController());
	}

	void createNew(EpptConfigurationController epptConfigurationController) throws IOException
	{
		NewProjectDialog nameDialog = new NewProjectDialog();
		String substring = String.valueOf(new Date().getTime()).substring(8);
		String newProjectName = "New_Project_" + substring;
		nameDialog.setTitle("New Project");
		nameDialog.setName(newProjectName);
		nameDialog.setDescription("");
		nameDialog.setVisible(true);
		if(!nameDialog.isCanceled())
		{
			Path projectPath = nameDialog.getProjectDirectory();
			Path projectFile = projectPath.resolve(nameDialog.getName() + "." + Constant.EPPT_EXT);
			epptConfigurationController.setProjectName(nameDialog.getName());
			epptConfigurationController.setProjectDescription(nameDialog.getDescription());
			ProjectConfigurationIO projectConfigurationIO = new ProjectConfigurationIO();
			projectConfigurationIO.saveConfiguration(epptConfigurationController, projectFile);
			EpptControllerProvider.setEpptController(projectFile);
			WindowManager.getDefault().getMainWindow().setTitle(
					Installer.MAIN_FRAME_NAME + " - " + EpptControllerProvider.getEpptConfigurationController().getProjectName());
			EpptControllerProvider.getEpptConfigurationController().clearModified();
			Savable.REGISTRY.lookupAll(ProjectConfigurationSavable.class)
							.forEach(ProjectConfigurationSavable::removeFromLookup);
		}
	}
}

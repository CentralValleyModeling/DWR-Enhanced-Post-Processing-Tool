/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */
package gov.ca.water.eppt.nbui.actions;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.swing.*;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.eppt.nbui.EpptControllerProvider;
import gov.ca.water.eppt.nbui.projectconfig.ProjectConfigurationIO;
import gov.ca.water.calgui.project.EpptConfigurationController;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.WindowManager;

import hec.heclib.dss.HecDSSFileAccess;

import static java.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static java.util.stream.Collectors.toList;

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
		EpptConfigurationController epptConfigurationController = EpptControllerProvider.getEpptConfigurationController();
		NewProjectDialog newProjectDialog = new NewProjectDialog();
		String newProjectName = epptConfigurationController.getProjectName() + " (1)";
		newProjectDialog.setTitle("Clone Project");
		newProjectDialog.setName(newProjectName);
		newProjectDialog.setDescription(epptConfigurationController.getProjectDescription());
		newProjectDialog.setVisible(true);
		if(!newProjectDialog.isCanceled())
		{
			Path lastConfigurationParentFolder = EpptPreferences.getLastProjectConfiguration().getParent();
			Path projectPath = newProjectDialog.getProjectDirectory();
			HecDSSFileAccess.closeAllFiles();
			try(Stream<Path> walk = Files.walk(lastConfigurationParentFolder))
			{
				List<Path> collect = walk.collect(toList());
				Frame mainWindow = WindowManager.getDefault().getMainWindow();
				JDialog jDialog = new JDialog(mainWindow, "Copying...", true);
				JProgressBar jProgressBar = new JProgressBar(0, collect.size());
				jProgressBar.setStringPainted(true);
				jDialog.add(jProgressBar, BorderLayout.CENTER);
				jDialog.pack();
				jDialog.setSize(790, 72);
				jDialog.setLocationRelativeTo(mainWindow);
				List<Path> filesToSkip = epptConfigurationController.getScenarioRuns()
																  .stream()
																  .map(EpptScenarioRun::getPostProcessDss)
																  .filter(Objects::nonNull)
																  .collect(java.util.stream.Collectors.toList());
				Path reportsDir = lastConfigurationParentFolder.resolve("Reports");
				if(reportsDir.toFile().exists())
				{
					try(Stream<Path> reports = Files.walk(reportsDir))
					{
						filesToSkip.addAll(reports.collect(toList()));
					}
				}
				CompletableFuture.runAsync(() ->
				{
					AtomicInteger i = new AtomicInteger(0);
					collect.stream()
						   .filter(s -> !filesToSkip.contains(s))
						   .forEach(source -> copy(source, projectPath.resolve(lastConfigurationParentFolder.relativize(source)),
								   jProgressBar, i));
				}).whenComplete((b, t) ->
				{
					jDialog.setVisible(false);
					if(t != null)
					{
						LOGGER.log(Level.SEVERE, "Error copying project", t);
					}
				});
				jDialog.setVisible(true);
			}
			ProjectConfigurationIO projectConfigurationIO = new ProjectConfigurationIO();
			Path projectFile = projectPath.resolve(newProjectDialog.getName() + ".eppt");
			epptConfigurationController.setProjectName(newProjectDialog.getName());
			epptConfigurationController.setProjectDescription(newProjectDialog.getDescription());
			projectConfigurationIO.saveConfiguration(epptConfigurationController, projectFile);
			EpptControllerProvider.setEpptController(projectFile);
			SaveProjectConfiguration.clearSaveCookie();
		}
	}

	private static void copy(Path source, Path dest, JProgressBar jProgressBar, AtomicInteger i)
	{
		try
		{
			if(!dest.toFile().exists() && !source.getFileName().toString().endsWith(".eppt"))
			{
				SwingUtilities.invokeLater(() ->
				{
					jProgressBar.setToolTipText("Copying: " + source);
					jProgressBar.setString("Copying: " + source.getFileName());
					jProgressBar.setValue(i.getAndAdd(1));
				});
				boolean mkdirs = dest.toFile().mkdirs();
				if(!mkdirs)
				{
					throw new IOException("Unable to create directory for: " + dest);
				}
				else if(source.toFile().isFile())
				{
					Files.copy(source, dest, REPLACE_EXISTING);
				}
			}
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Error copying file: " + source, e);
		}
	}
}

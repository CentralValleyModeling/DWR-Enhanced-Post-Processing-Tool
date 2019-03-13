/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.awt.Dimension;
import java.awt.Frame;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bus_service.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.quickresults.ui.scenarioconfig.ProjectConfigurationPanel;
import org.openide.modules.ModuleInstall;
import org.openide.windows.WindowManager;

import rma.swing.logging.DialogLogHandler;

public class Installer extends ModuleInstall
{
	public static final String MAIN_FRAME_NAME = "EPPT";
	private static final Logger LOGGER = Logger.getLogger(Installer.class.getName());

	@Override
	public void restored()
	{
		initEpptHome();
		initEpptConfigs();
		initHeclibDll();
		initLogger();
		loadLastProjectConfiguration();
		setMinimumWindowSize();
		initPlotHandler();
	}

	private void setMinimumWindowSize()
	{
		WindowManager.getDefault().invokeWhenUIReady(() ->
		{
			WindowManager.getDefault().getMainWindow().setMinimumSize(new Dimension(545, 630));
		});
	}
	
	private void loadLastProjectConfiguration()
	{
		WindowManager.getDefault().invokeWhenUIReady(() ->
		{
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			Path lastProjectConfiguration = EpptPreferences.getLastProjectConfiguration();
			try
			{
				projectConfigurationPanel.loadProjectConfiguration(lastProjectConfiguration);
				WindowManager.getDefault().getMainWindow().setTitle(
						MAIN_FRAME_NAME + " - " + projectConfigurationPanel.getProjectName());
			}
			catch(IOException ex)
			{
				LOGGER.log(Level.SEVERE,
						"Unable to load last Project Configuration EPPT Home: " + lastProjectConfiguration, ex);
			}
		});
	}
	private void initPlotHandler()
	{
		WindowManager.getDefault().invokeWhenUIReady(() ->
		{
			Frame mainWindow = WindowManager.getDefault().getMainWindow();
			DialogSvcImpl.installMainFrame((JFrame) mainWindow);
		});
		DisplayHelper.installPlotHandler(new TopComponentPlotHandler());
	}
	private void initEpptHome()
	{
		createPaths(EpptPreferences.getProjectsPath());
		createPaths(EpptPreferences.getModelDssPath());
		createPaths(EpptPreferences.getScenariosPaths());
		createPaths(EpptPreferences.getReportsPath());
	}

	private void createPaths(Path path)
	{
		if(!path.toFile().exists())
		{
			try
			{
				Files.createDirectories(path);
			}
			catch(IOException ex)
			{
				LOGGER.log(Level.SEVERE, "Unable to initialize EPPT Home: " + path, ex);
			}
		}
	}


	private void initEpptConfigs()
	{
		try
		{
			GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		}
		catch(EpptInitializationException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to initialize GUI Links", ex);
		}
	}

	private void initHeclibDll()
	{
		String pathToAdd = "eppt/modules/lib";
		try
		{

			final Field usrPathsField = ClassLoader.class.getDeclaredField("usr_paths");
			usrPathsField.setAccessible(true);

			//get array of paths
			final String[] paths = (String[]) usrPathsField.get(null);

			//check if the path to add is already present
			for(String path : paths)
			{
				if(path.equals(pathToAdd))
				{
					return;
				}
			}

			//add the new path
			final String[] newPaths = Arrays.copyOf(paths, paths.length + 1);
			newPaths[newPaths.length - 1] = pathToAdd;
			usrPathsField.set(null, newPaths);
			System.loadLibrary("javaHeclib");
		}
		catch(NoSuchFieldException | IllegalAccessException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to initialize javaHeclib.dll", ex);
		}
	}

	private void initLogger()
	{
		//Root logger
		Logger rootLogger = Logger.getLogger("");

		//nix the netbeans logger
		Logger netbeansLogger = Logger.getLogger("org.netbeans.core");
		netbeansLogger.setParent(rootLogger);
		netbeansLogger.setUseParentHandlers(true);

		WindowManager.getDefault().invokeWhenUIReady(() ->
		{
			WindowManager.getDefault().getMainWindow();
			String publishedLevel = Level.SEVERE.getName();
			String showDialogLevel = Level.SEVERE.getName();

			DialogLogHandler handler = DialogLogHandler.getInstance();
			handler.addIgnoredLoggerName("netbeans");
			handler.addIgnoredSourceClass("org.netbeans");
			handler.addIgnoredSourceClass("org.openide");

			handler.setLevel(Level.parse(publishedLevel));
			handler.setShowDialogLevel(Level.parse(showDialogLevel));

			handler.setFrame(true);
			handler.setTitle("EPPT Error Log");
			handler.setParentWindow(WindowManager.getDefault().getMainWindow());

			Handler[] handlers = rootLogger.getHandlers();
			for(Handler defaultHandlers : handlers)
			{
				if(defaultHandlers.getClass().getName().contains("TopLogging"))
				{
					rootLogger.removeHandler(defaultHandlers);
				}
			}
			rootLogger.addHandler(handler);
		});
	}
}

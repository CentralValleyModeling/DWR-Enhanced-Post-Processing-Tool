/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bus_service.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.presentation.DisplayFrame;
import org.openide.modules.ModuleInstall;
import org.openide.windows.WindowManager;

import rma.swing.logging.DialogLogHandler;

public class Installer extends ModuleInstall
{
	private static final Logger LOGGER = Logger.getLogger(Installer.class.getName());

	@Override
	public void restored()
	{
		initEpptConfigs();
		initHeclibDll();
		initLogger();
		initPlotHandler();
	}

	private void initPlotHandler()
	{
		DisplayFrame.installPlotHandler(new TopComponentPlotHandler());
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
			handler.setTitle("REGI Error Log");
			handler.setParentWindow(WindowManager.getDefault().getMainWindow());

			rootLogger.addHandler(handler);
		});
	}
}

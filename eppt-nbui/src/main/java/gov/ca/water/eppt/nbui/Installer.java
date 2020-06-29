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
import gov.ca.water.calgui.busservice.impl.DetailedIssuesReader;
import gov.ca.water.calgui.busservice.impl.DssPatternUpdater;
import gov.ca.water.calgui.busservice.impl.EpptParameters;
import gov.ca.water.calgui.busservice.impl.EpptReportingMonths;
import gov.ca.water.calgui.busservice.impl.ErrorValueFlags;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.ScriptedEpptStatistics;
import gov.ca.water.calgui.busservice.impl.ThresholdLinksSeedDataSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearIndexReader;
import gov.ca.water.calgui.busservice.impl.WaterYearPeriodReader;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.eppt.nbui.actions.RunWreslScript;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import org.openide.modules.ModuleInstall;
import org.openide.windows.WindowManager;

import hec.heclib.dss.HecDSSFileAccess;
import hec.heclib.dss.HecDataManager;
import hec.heclib.util.Heclib;
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

	@Override
	public boolean closing()
	{
		RunWreslScript.destroyProcesses();
		HecDataManager.closeAllFiles();
		return true;
	}

	private void setMinimumWindowSize()
	{
		WindowManager.getDefault().invokeWhenUIReady(() -> WindowManager.getDefault().getMainWindow().setMinimumSize(new Dimension(545, 630)));
	}

	private void loadLastProjectConfiguration()
	{
		Path lastProjectConfiguration = EpptPreferences.getLastProjectConfiguration();
		EpptControllerProvider.setEpptController(lastProjectConfiguration);
		WindowManager wm = WindowManager.getDefault();
		wm.invokeWhenUIReady(() -> wm.getMainWindow().setTitle(
				MAIN_FRAME_NAME + " - " + EpptControllerProvider.getEpptConfigurationController().getProjectName()));
	}

	private void initPlotHandler()
	{
		WindowManager wm = WindowManager.getDefault();
		wm.invokeWhenUIReady(() ->
		{
			Frame mainWindow = wm.getMainWindow();
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
			ThresholdLinksSeedDataSvc.createSeedDataSvcImplInstance();
			EpptReportingMonths.createTrendReportingMonthsInstance();
			WaterYearDefinitionSvc.createSeedDataSvcImplInstance();
			DetailedIssuesReader.createDetailedIssues();
			EpptParameters.createTrendReportingParametersInstance();
			ScriptedEpptStatistics.createScriptedStatistics();
			WaterYearIndexReader.createInstance();
			WaterYearPeriodReader.createInstance();
			DssPatternUpdater.initPatterns();
			ErrorValueFlags.initializeErrorFlags();
			//This is done so that the JFXPanels created on startup have already loaded up the Platform thread.
			Platform.setImplicitExit(false);
			new JFXPanel();
		}
		catch(EpptInitializationException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to initialize GUI or Threshold Links", ex);
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
			HecDSSFileAccess.setMessageLevel(HecDSSFileAccess.MESS_LEVEL_GENERAL);
			Heclib.Hec_zset("ALLV", "", 6);
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

		WindowManager.getDefault().invokeWhenUIReady(() -> setupLogHandlers(rootLogger));
	}

	private void setupLogHandlers(Logger rootLogger)
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
	}
}

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
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
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
import org.openide.awt.Actions;
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
	//java.util.logging only weakly references loggers, so levels set on them are lost unless the instances are held
	private static final List<Logger> QUIETED_LOGGERS = new ArrayList<>();

	@Override
	public void restored()
	{
		StdoutLogRedirect.install();
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
		try {
			System.loadLibrary("javaHeclib");
			HecDSSFileAccess.setMessageLevel(HecDSSFileAccess.MESS_LEVEL_GENERAL);
			Heclib.Hec_zset("ALLV", "", 6);
		} catch (Throwable t) {
			LOGGER.log(Level.SEVERE, "Failed to load heclib!", t);
		}
	}

	private void quietNetBeansLoggers()
	{
		silence("org.netbeans.core.startup.NbEvents", Level.WARNING);
		silence("org.netbeans.core.network.proxy", Level.WARNING);
		silence("org.netbeans.modules.options.keymap.LayersBridge", Level.SEVERE);
		silence("org.openide.awt.Toolbar", Level.WARNING);
		silence("org.netbeans.ui.metrics", Level.WARNING);
		silence("org.netbeans.core.ui.warmup.DiagnosticTask", Level.WARNING);
		silence("org.netbeans.modules.sampler.InternalSampler", Level.WARNING);
	}

	private static void silence(String loggerName, Level level)
	{
		Logger logger = Logger.getLogger(loggerName);
		logger.setLevel(level);
		QUIETED_LOGGERS.add(logger);
	}

	private void initLogger()
	{
		quietNetBeansLoggers();
		//Root logger
		Logger rootLogger = Logger.getLogger("");

		//nix the netbeans logger
		Logger netbeansLogger = Logger.getLogger("org.netbeans.core");
		netbeansLogger.setParent(rootLogger);
		netbeansLogger.setUseParentHandlers(true);

		WindowManager.getDefault().invokeWhenUIReady(() ->
		{
			setupLogHandlers(rootLogger);
			openIdeLog();
		});
	}

	private void openIdeLog()
	{
		Action logAction = Actions.forID("View", "org.netbeans.core.actions.LogAction");
		if(logAction != null)
		{
			logAction.actionPerformed(new ActionEvent(this, ActionEvent.ACTION_PERFORMED, ""));
		}
		else
		{
			LOGGER.log(Level.WARNING, "IDE Log action not found");
		}
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

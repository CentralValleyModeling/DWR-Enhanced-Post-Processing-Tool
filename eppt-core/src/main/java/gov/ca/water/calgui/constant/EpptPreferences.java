/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.constant;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.prefs.Preferences;
import javax.swing.filechooser.FileSystemView;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-08-2019
 */
public final class EpptPreferences
{
	private static final Preferences ROOT_PREFERENCES = Preferences.userRoot().node("DWR").node("EPPT");
	private static final Preferences REPORT_NODE = ROOT_PREFERENCES.node("report");
	private static final Preferences EPPT_HOME = ROOT_PREFERENCES.node("eppt_home");
	private static final String PROJECT_DIRECTORY = "project_directory";
	private static final String LAST_SCENARIO_CONFIGURATION = "last_scenario_configuration";
	private static final String REPORT_OUTPUT_LOCATION = "report_output_location";

	private EpptPreferences()
	{
		throw new AssertionError("Utility class");
	}

	public static Path getProjectsPath()
	{
		String retval = "";
		File defaultDirectory = FileSystemView.getFileSystemView().getDefaultDirectory();
		if(defaultDirectory != null)
		{

			String epptHomeDefault = Paths.get(defaultDirectory.getPath()).resolve("EPPT").toString();
			retval = EPPT_HOME.get(PROJECT_DIRECTORY, epptHomeDefault);
		}
		return Paths.get(retval);
	}

	public static Path getScenariosPaths()
	{
		return getProjectsPath().resolve(Constant.SCENARIOS_DIR);
	}

	public static Path getModelDssPath()
	{
		return getProjectsPath().resolve(Constant.MODEL_DIR).resolve(Constant.MODEL_DSS_DIR);
	}

	public static Path getReportsPath()
	{
		return getProjectsPath().resolve(Constant.REPORTS_DIR);
	}

	public static void setProjectsPath(String text)
	{
		EPPT_HOME.put(PROJECT_DIRECTORY, text);
	}

	public static Path getLastProjectConfiguration()
	{
		String scenarioConfigurationFile = EPPT_HOME.get(LAST_SCENARIO_CONFIGURATION, "");
		return Paths.get(scenarioConfigurationFile);
	}

	public static void setLastProjectConfiguration(Path lastConfiguration)
	{
		EPPT_HOME.put(LAST_SCENARIO_CONFIGURATION, lastConfiguration.toAbsolutePath().toString());
	}

	public static String getResultsOutputLocation()
	{
		return REPORT_NODE.get(REPORT_OUTPUT_LOCATION, "editor");
	}

	public static void setResultsOutputLocation(String path)
	{
		REPORT_NODE.put(REPORT_OUTPUT_LOCATION, path);
	}
}

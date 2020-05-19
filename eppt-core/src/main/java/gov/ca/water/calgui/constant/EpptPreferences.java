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
	private static final Preferences WRIMS_NODE = ROOT_PREFERENCES.node("wrims");
	private static final Preferences EPPT_HOME = ROOT_PREFERENCES.node("eppt_home");
	private static final String PROJECT_DIRECTORY = "project_directory";
	private static final String WRIMS_DIRECTORY = "wrims_directory";
	private static final String LAST_SCENARIO_CONFIGURATION = "last_scenario_configuration";
	private static final String REPORT_OUTPUT_LOCATION = "report_output_location";
	private static final String USERNAME = "eppt_username";
	private static final String USE_PLOTLY_PREF = "use-plotly-pref";
	private static final String AUTO_REFRESH_TREND_REPORT = "auto-refresh-trendreport";

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
			retval = EPPT_HOME.get(PROJECT_DIRECTORY, getDefaultProjectPath().toString());
		}
		return Paths.get(retval);
	}

	public static Path getDefaultProjectPath()
	{
		File defaultDirectory = FileSystemView.getFileSystemView().getDefaultDirectory();
		return Paths.get(defaultDirectory.getPath()).resolve("EPPT");
	}

	public static void setProjectsPath(String text)
	{
		EPPT_HOME.put(PROJECT_DIRECTORY, text);
	}

	public static void removeProjectsPathPreference()
	{
		EPPT_HOME.remove(PROJECT_DIRECTORY);
	}

	public static Path getScenariosPaths()
	{
		return getProjectsPath().resolve(Constant.SCENARIOS_DIR);
	}

	public static Path getModelDssPath()
	{
		return getProjectsPath().resolve(Constant.MODEL_DIR).resolve(Constant.MODEL_DSS_DIR);
	}

	public static Path getWreslDirectory()
	{
		return Paths.get(Constant.WRESL_DIR);
	}

	public static Path getReportsPath()
	{
		return getProjectsPath().resolve(Constant.REPORTS_DIR);
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
		return REPORT_NODE.get(REPORT_OUTPUT_LOCATION, getDefaultResultsOutputLocation());
	}

	public static String getDefaultResultsOutputLocation()
	{
		return "editor";
	}

	public static void setResultsOutputLocation(String path)
	{
		REPORT_NODE.put(REPORT_OUTPUT_LOCATION, path);
	}

	public static void removeResultsOutputLocation()
	{
		REPORT_NODE.remove(REPORT_OUTPUT_LOCATION);
	}

	public static String getUsername()
	{
		return EPPT_HOME.get(USERNAME, System.getProperty("user.name"));
	}

	public static void setUsername(String path)
	{
		EPPT_HOME.put(USERNAME, path);
	}

	public static Path getWrimsPath()
	{
		return Paths.get(WRIMS_NODE.get(WRIMS_DIRECTORY, getDefaultWrimsPath().toString()));
	}

	public static Path getDefaultWrimsPath()
	{
		return Paths.get(Constant.WRIMS_DIR);
	}

	public static void setWrimsPath(Path path)
	{
		WRIMS_NODE.put(WRIMS_DIRECTORY, path.toString());
	}

	public static void removeWrimsPathPreference()
	{
		WRIMS_NODE.remove(WRIMS_DIRECTORY);
	}

	public static boolean usePlotly()
	{
		return WRIMS_NODE.getBoolean(USE_PLOTLY_PREF, false);
	}

	public static void setUsePlotly(boolean selected)
	{
		WRIMS_NODE.putBoolean(USE_PLOTLY_PREF, selected);
	}

	public static boolean getAutoRefreshTrendReport()
	{
		return EPPT_HOME.getBoolean(AUTO_REFRESH_TREND_REPORT, false);
	}

	public static void setAutoRefreshTrendReport(boolean autoRefresh)
	{
		EPPT_HOME.putBoolean(AUTO_REFRESH_TREND_REPORT, autoRefresh);
	}
}

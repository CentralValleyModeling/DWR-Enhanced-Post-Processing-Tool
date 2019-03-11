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
	private static final String EPPT_HOME = "eppt_home";
	private static final String PROJECT_DIRECTORY = "project_directory";
	private static final String LAST_SCENARIO_CONFIGURATION = "last_scenario_configuration";

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
			Preferences homePrefs = ROOT_PREFERENCES.node(EPPT_HOME);
			retval = homePrefs.get(PROJECT_DIRECTORY, epptHomeDefault);
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

	public static Path getLastScenarioConfiguration()
	{
		String scenarioConfigurationFile = ROOT_PREFERENCES.node(EPPT_HOME).get(LAST_SCENARIO_CONFIGURATION, "");
		return Paths.get(scenarioConfigurationFile);
	}

	public static void setLastScenarioConfiguration(Path lastConfiguration)
	{
		ROOT_PREFERENCES.node(EPPT_HOME).put(LAST_SCENARIO_CONFIGURATION,
				lastConfiguration.toAbsolutePath().toString());
	}

}

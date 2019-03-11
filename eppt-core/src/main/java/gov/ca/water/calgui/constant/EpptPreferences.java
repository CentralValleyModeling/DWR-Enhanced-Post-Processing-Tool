/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.constant;

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

	private EpptPreferences()
	{
		throw new AssertionError("Utility class");
	}

	public static Path getProjectsPath()
	{
		String epptHomeDefault = Paths.get(FileSystemView.getFileSystemView().getDefaultDirectory().getPath()).resolve(
				"EPPT").toString();
		Preferences homePrefs = ROOT_PREFERENCES.node(EPPT_HOME);
		String homeDirPref = homePrefs.get(PROJECT_DIRECTORY, epptHomeDefault);
		return Paths.get(homeDirPref);
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

}

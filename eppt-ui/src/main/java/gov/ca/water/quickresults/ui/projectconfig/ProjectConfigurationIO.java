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

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.Component;
import java.awt.Container;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptProject;
import org.jfree.data.time.Month;
import org.json.JSONArray;
import org.json.JSONObject;
import org.swixml.SwingEngine;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-11-2019
 */
class ProjectConfigurationIO
{
	static final String START_MONTH_PROPERTY = "Start_Month";
	static final String START_YEAR_PROPERTY = "Start_Year";
	static final String END_MONTH_PROPERTY = "End_Month";
	static final String END_YEAR_PROPERTY = "End_Year";
	static final String VERSION_KEY = "version";
	static final String VERSION_1_0 = "1.0";
	static final String VERSION_2_0 = "2.0";
	static final String NAME_KEY = "name";
	static final String DESCRIPTION_KEY = "description";
	static final String CREATION_DATE_KEY = "creation_date";
	static final String SCENARIO_FILES_KEY = "scenario_files";
	static final String SCENARIOS_KEY = "scenarios";
	static final String SCENARIO_NAME = "scenario_name";
	static final String SCENARIO_DESCRIPTION = "scenario_description";
	static final String SCENARIO_MODEL = "scenario_model";
	static final String SCENARIO_OUTPUT_PATH = "scenario_output_path";
	static final String SCENARIO_WRESL_MAIN = "scenario_wresl_main";
	static final String SCENARIO_DSS_FILES = "scenario_dss_files";
	static final String SCENARIO_DSS_NAME = "scneario_dss_name";
	static final String SCENARIO_DSS_PATH = "scneario_dss_path";
	static final String SCENARIO_DV_KEY = "scenario_dv_key";
	static final String SCENARIO_SV_KEY = "scenario_sv_key";
	static final String SCENARIO_IV_KEY = "scenario_iv_key";
	static final String SCENARIO_DSS_EXTRA = "scenario_dss_extra";
	static final String FILE_KEY = "file";
	static final String MODEL_KEY = "model";
	static final String SELECTED_KEY = "selected";
	static final String DISPLAY_OPTIONS_KEY = "display_options";
	static final String MONTH_OPTIONS_KEY = "month_options";
	static final String ID_KEY = "id";

	void saveConfiguration(Path selectedPath, String name, String description) throws IOException
	{
		ProjectConfigurationIOVersion2 projectConfigurationIOVersion2 = new ProjectConfigurationIOVersion2();
		projectConfigurationIOVersion2.saveConfiguration(selectedPath, name, description);
	}

	EpptProject loadConfiguration(Path selectedPath) throws IOException
	{
		String collect = String.join("\n", Files.readAllLines(selectedPath));
		if(collect != null && !collect.isEmpty())
		{
			JSONObject jsonObject = new JSONObject(collect);
			if(VERSION_1_0.equalsIgnoreCase(jsonObject.getString(VERSION_KEY)))
			{
				ProjectConfigurationIOVersion1 projectConfigurationIOVersion1 = new ProjectConfigurationIOVersion1();
				return projectConfigurationIOVersion1.loadConfiguration(selectedPath, jsonObject);
			}
			else if(VERSION_2_0.equalsIgnoreCase(jsonObject.getString(VERSION_KEY)))
			{
				ProjectConfigurationIOVersion2 projectConfigurationIOVersion1 = new ProjectConfigurationIOVersion2();
				return projectConfigurationIOVersion1.loadConfiguration(jsonObject);
			}
			else
			{
				throw new IOException("Version of EPPT Project not supported: " + jsonObject.getString(VERSION_KEY));
			}
		}
		else
		{
			throw new IOException("Corrupt EPPT Project file: " + selectedPath);
		}
	}
}

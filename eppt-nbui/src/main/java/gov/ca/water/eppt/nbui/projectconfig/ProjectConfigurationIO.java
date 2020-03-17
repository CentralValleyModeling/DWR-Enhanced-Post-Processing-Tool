/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.eppt.nbui.projectconfig;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import gov.ca.water.calgui.project.EpptProject;
import gov.ca.water.calgui.project.EpptConfigurationController;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-11-2019
 */
public class ProjectConfigurationIO
{
	static final String START_YEAR_PROPERTY = "Start_Year";
	static final String END_YEAR_PROPERTY = "End_Year";
	static final String VERSION_KEY = "version";
	private static final String VERSION_1_0 = "1.0";
	static final String VERSION_2_0 = "2.0";
	static final String VERSION_3_0 = "3.0";
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
	static final String SCENARIO_WRESL_DIR = "scenario_wresl_dir";
	static final String SCENARIO_DSS_FILES = "scenario_dss_files";
	static final String SCENARIO_DSS_NAME = "scneario_dss_name";
	static final String SCENARIO_DSS_PATH = "scneario_dss_path";
	static final String SCENARIO_A_PART = "scneario_a_part";
	static final String SCENARIO_E_PART = "scneario_e_part";
	static final String SCENARIO_F_PART = "scneario_f_part";
	static final String SCENARIO_DV_KEY = "scenario_dv_key";
	static final String SCENARIO_SV_KEY = "scenario_sv_key";
	static final String SCENARIO_IV_KEY = "scenario_iv_key";
	static final String SCENARIO_DTS_KEY = "scenario_dts_key";
	static final String BASE_SELECTED_KEY = "base_selected_key";
	static final String ALT_SELECTED_KEY = "alt_selected_key";
	static final String SCENARIO_COLOR_KEY = "scenario_color";
	static final String SCENARIO_DSS_EXTRA = "scenario_dss_extra";
	static final String SCENARIO_WATER_TABLE = "scenario_water_table";
	static final String SCENARIO_LOOKUP_DIR = "scenario_water_lookup_dir";
	static final String FILE_KEY = "file";
	static final String MODEL_KEY = "model";
	static final String SELECTED_KEY = "selected";
	static final String DISPLAY_OPTIONS_KEY = "display_options";
	static final String MONTH_OPTIONS_KEY = "month_options";
	static final String ID_KEY = "id";

	public void saveConfiguration(EpptConfigurationController epptConfigurationController, Path selectedPath) throws IOException
	{
		ProjectConfigurationIOVersion3 projectConfigurationIOVersion3 = new ProjectConfigurationIOVersion3();
		projectConfigurationIOVersion3.saveConfiguration(selectedPath, epptConfigurationController);
	}

	public EpptProject loadConfiguration(Path selectedPath) throws IOException
	{
		String collect = String.join("\n", Files.readAllLines(selectedPath));
		if(!collect.isEmpty())
		{
			JSONObject jsonObject = new JSONObject(collect);
			if(VERSION_1_0.equalsIgnoreCase(jsonObject.getString(VERSION_KEY)))
			{
				ProjectConfigurationIOVersion1 projectConfigurationIOVersion1 = new ProjectConfigurationIOVersion1();
				return projectConfigurationIOVersion1.loadConfiguration(selectedPath, jsonObject);
			}
			else if(VERSION_2_0.equalsIgnoreCase(jsonObject.getString(VERSION_KEY)))
			{
				ProjectConfigurationIOVersion2 projectConfigurationIOVersion2 = new ProjectConfigurationIOVersion2();
				return projectConfigurationIOVersion2.loadConfiguration(jsonObject, selectedPath);
			}
			else if(VERSION_3_0.equalsIgnoreCase(jsonObject.getString(VERSION_KEY)))
			{
				ProjectConfigurationIOVersion3 projectConfigurationIOVersion3 = new ProjectConfigurationIOVersion3();
				return projectConfigurationIOVersion3.loadConfiguration(jsonObject, selectedPath);
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
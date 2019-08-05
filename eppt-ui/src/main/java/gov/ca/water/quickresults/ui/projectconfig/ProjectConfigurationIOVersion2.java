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
import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptProject;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import org.jfree.data.time.Month;
import org.json.JSONArray;
import org.json.JSONObject;

import static gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationIO.*;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-06-2019
 */
class ProjectConfigurationIOVersion2
{
	void saveConfiguration(Path selectedPath, String name, String description) throws IOException
	{
		try(BufferedWriter bufferedWriter = Files.newBufferedWriter(selectedPath))
		{
			JSONObject jsonObject = new JSONObject();
			jsonObject.put(VERSION_KEY, VERSION_2_0);
			jsonObject.put(NAME_KEY, name);
			jsonObject.put(DESCRIPTION_KEY, description);
			jsonObject.put(CREATION_DATE_KEY, ZonedDateTime.now());
			jsonObject.put(SCENARIOS_KEY, buildScenarioFilesArray());
			jsonObject.put(DISPLAY_OPTIONS_KEY, writeSelectedProperties());
			jsonObject.put(MONTH_OPTIONS_KEY, writeMonthProperties());
			jsonObject.write(bufferedWriter, 4, 4);
		}
	}

	private JSONArray writeSelectedProperties()
	{
		JSONArray jsonArray = new JSONArray();
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		Component[] components = projectConfigurationPanel.getComponents();
		writeSelectedProperties(jsonArray, components);
		return jsonArray;
	}

	private JSONObject writeMonthProperties()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();

		JSONObject jsonObject = new JSONObject();
		jsonObject.put(START_MONTH_PROPERTY,
				String.valueOf(projectConfigurationPanel.getStartMonth().getMonth().getValue()));
		jsonObject.put(START_YEAR_PROPERTY,
				String.valueOf(projectConfigurationPanel.getStartMonth().getYear()));
		jsonObject.put(END_MONTH_PROPERTY, String.valueOf(projectConfigurationPanel.getEndMonth().getMonth().getValue()));
		jsonObject.put(END_YEAR_PROPERTY, String.valueOf(projectConfigurationPanel.getEndMonth().getYear()));

		return jsonObject;
	}

	private JSONArray buildScenarioFilesArray()
	{
		JSONArray jsonArray = new JSONArray();
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		List<EpptScenarioRun> epptScenarioRuns = projectConfigurationPanel.getAllEpptScenarioRuns();
		for(EpptScenarioRun scenario : epptScenarioRuns)
		{
			JSONObject jsonObject = new JSONObject();
			jsonObject.put(SCENARIO_NAME, scenario.getName());
			jsonObject.put(SCENARIO_DESCRIPTION, scenario.getDescription());
			jsonObject.put(SCENARIO_MODEL, scenario.getModel().toString());
			jsonObject.put(SCENARIO_OUTPUT_PATH, scenario.getOutputPath());
			jsonObject.put(SCENARIO_WRESL_MAIN, scenario.getWreslMain());
			EpptDssContainer dssContainer = scenario.getDssContainer();
			JSONObject dssContainerJson = new JSONObject();
			JSONObject namedDssDvJsonObject = createDssJson(dssContainer.getDvDssFile());
			JSONObject namedDssSvJsonObject = createDssJson(dssContainer.getSvDssFile());
			JSONObject namedDssIvJsonObject = createDssJson(dssContainer.getIvDssFile());
			JSONObject namedDssDtsJsonObject = createDssJson(dssContainer.getDtsDssFile());
			if(namedDssDvJsonObject != null)
			{
				dssContainerJson.put(SCENARIO_DV_KEY, namedDssDvJsonObject);
			}
			if(namedDssSvJsonObject != null)
			{
				dssContainerJson.put(SCENARIO_SV_KEY, namedDssSvJsonObject);
			}
			if(namedDssIvJsonObject != null)
			{
				dssContainerJson.put(SCENARIO_IV_KEY, namedDssIvJsonObject);
			}
			if(namedDssDtsJsonObject != null)
			{
				dssContainerJson.put(SCENARIO_DTS_KEY, namedDssDtsJsonObject);
			}
			Path waterYearTable = scenario.getWaterYearTable();
			if(waterYearTable != null)
			{
				jsonObject.put(SCENARIO_WATER_TABLE, waterYearTable.toString());
			}
			JSONArray extraDssArray = new JSONArray();
			dssContainer.getExtraDssFiles().stream()
						.map(this::createDssJson)
						.filter(Objects::nonNull)
						.forEach(extraDssArray::put);
			dssContainerJson.put(SCENARIO_DSS_EXTRA, extraDssArray);
			jsonObject.put(SCENARIO_DSS_FILES, dssContainerJson);
			jsonArray.put(jsonObject);
		}
		return jsonArray;
	}

	private List<EpptScenarioRun> readEpptScenarioRuns(JSONArray scenarioPaths)
	{
		List<EpptScenarioRun> retval = new ArrayList<>();
		for(int i = 0; i < scenarioPaths.length(); i++)
		{
			JSONObject scenarioJson = scenarioPaths.getJSONObject(i);
			String name = scenarioJson.getString(SCENARIO_NAME);
			String description = scenarioJson.getString(SCENARIO_DESCRIPTION);
			GUILinksAllModelsBO.Model model = GUILinksAllModelsBO.Model.findModel(
					scenarioJson.getString(SCENARIO_MODEL));
			Path outputPath = Paths.get(scenarioJson.getString(SCENARIO_OUTPUT_PATH));
			Path wreslMain = Paths.get(scenarioJson.getString(SCENARIO_WRESL_MAIN));
			Path waterYearTable = Paths.get(Constant.WY_TYPES_TABLE);
			if(scenarioJson.has(SCENARIO_WATER_TABLE))
			{
				waterYearTable = Paths.get(scenarioJson.getString(SCENARIO_WATER_TABLE));
			}
			JSONObject jsonObject = scenarioJson.getJSONObject(SCENARIO_DSS_FILES);
			NamedDssPath dvDssFile = null;
			if(jsonObject.has(SCENARIO_DV_KEY))
			{
				dvDssFile = readDssJson(jsonObject.getJSONObject(SCENARIO_DV_KEY));
			}
			NamedDssPath svDssFile = null;
			if(jsonObject.has(SCENARIO_SV_KEY))
			{
				svDssFile = readDssJson(jsonObject.getJSONObject(SCENARIO_SV_KEY));
			}
			NamedDssPath ivDssFile = null;
			if(jsonObject.has(SCENARIO_IV_KEY))
			{
				ivDssFile = readDssJson(jsonObject.getJSONObject(SCENARIO_IV_KEY));
			}
			NamedDssPath dtsDssFile = null;
			if(jsonObject.has(SCENARIO_DTS_KEY))
			{
				dtsDssFile = readDssJson(jsonObject.getJSONObject(SCENARIO_DTS_KEY));
			}

			List<NamedDssPath> extraDssFiles = readExtraDss(jsonObject);
			EpptDssContainer dssContainer = new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, dtsDssFile, extraDssFiles);
			EpptScenarioRun epptScenarioRun = new EpptScenarioRun(name, description,
					model, outputPath, wreslMain, waterYearTable, dssContainer);
			retval.add(epptScenarioRun);
		}
		return retval;
	}

	private List<NamedDssPath> readExtraDss(JSONObject jsonObject)
	{
		List<NamedDssPath> extraDssFiles = new ArrayList<>();
		if(jsonObject.has(SCENARIO_DSS_EXTRA))
		{
			JSONArray jsonArray = jsonObject.getJSONArray(SCENARIO_DSS_EXTRA);
			for(int j = 0; j < jsonArray.length(); j++)
			{
				extraDssFiles.add(readDssJson(jsonArray.getJSONObject(j)));
			}
		}
		return extraDssFiles;
	}

	private NamedDssPath readDssJson(JSONObject dssJson)
	{
		String aliasName = dssJson.getString(SCENARIO_DSS_NAME);
		Path dssPath = Paths.get(dssJson.getString(SCENARIO_DSS_PATH));
		String aPart = "";
		if(dssJson.has(SCENARIO_A_PART))
		{
			aPart = dssJson.getString(SCENARIO_A_PART);
		}
		String ePart = "";
		if(dssJson.has(SCENARIO_E_PART))
		{
			ePart = dssJson.getString(SCENARIO_E_PART);
		}
		String fPart = "";
		if(dssJson.has(SCENARIO_F_PART))
		{
			fPart = dssJson.getString(SCENARIO_F_PART);
		}
		return new NamedDssPath(dssPath, aliasName, aPart, ePart, fPart);
	}

	private JSONObject createDssJson(NamedDssPath dssFile)
	{
		JSONObject namedDssJsonObject = null;
		if(dssFile != null)
		{
			namedDssJsonObject = new JSONObject();
			if(dssFile.getAliasName() != null)
			{
				namedDssJsonObject.put(SCENARIO_DSS_NAME, dssFile.getAliasName());
			}
			if(dssFile.getDssPath() != null)
			{
				namedDssJsonObject.put(SCENARIO_DSS_PATH, dssFile.getDssPath());
			}
			if(dssFile.getAPart() != null)
			{
				namedDssJsonObject.put(SCENARIO_A_PART, dssFile.getAPart());
			}
			if(dssFile.getEPart() != null)
			{
				namedDssJsonObject.put(SCENARIO_E_PART, dssFile.getEPart());
			}
			if(dssFile.getFPart() != null)
			{
				namedDssJsonObject.put(SCENARIO_F_PART, dssFile.getFPart());
			}
		}
		return namedDssJsonObject;
	}

	private void writeSelectedProperties(JSONArray jsonArray, Component[] components)
	{
		for(Component component : components)
		{
			if(component instanceof JCheckBox)
			{
				JSONObject checkBox = new JSONObject();
				checkBox.put(ID_KEY, component.getName());
				checkBox.put(SELECTED_KEY, ((JCheckBox) component).isSelected());
				jsonArray.put(checkBox);
			}
			else if(component instanceof JRadioButton)
			{
				JSONObject radioButton = new JSONObject();
				radioButton.put(ID_KEY, component.getName());
				radioButton.put(SELECTED_KEY, ((JRadioButton) component).isSelected());
				jsonArray.put(radioButton);
			}
			else if(component instanceof Container)
			{
				writeSelectedProperties(jsonArray, ((Container) component).getComponents());
			}
		}
	}

	private LocalDate readStartMonthProperties(JSONObject jsonObject)
	{
		String startMonth = jsonObject.getString(START_MONTH_PROPERTY);
		String startYear = jsonObject.getString(START_YEAR_PROPERTY);
		return LocalDate.of(Integer.parseInt(startYear), Integer.parseInt(startMonth), 1);
	}

	private LocalDate readEndMonthProperties(JSONObject jsonObject)
	{
		String endMonth = jsonObject.getString(END_MONTH_PROPERTY);
		String endYear = jsonObject.getString(END_YEAR_PROPERTY);
		return (LocalDate) TemporalAdjusters.lastDayOfMonth().adjustInto(LocalDate.of(Integer.parseInt(endYear), Integer.parseInt(endMonth), 1));
	}

	private Map<String, Boolean> readDisplayProperties(JSONArray jsonArray)
	{
		Map<String, Boolean> retval = new HashMap<>();
		for(int i = 0; i < jsonArray.length(); i++)
		{
			JSONObject jsonObject = jsonArray.getJSONObject(i);
			if(jsonObject != null && jsonObject.getString(ID_KEY) != null)
			{
				boolean selected = jsonObject.getBoolean(SELECTED_KEY);
				retval.put(jsonObject.getString(ID_KEY), selected);
			}
		}
		return retval;
	}

	EpptProject loadConfiguration(JSONObject jsonObject)
	{
		JSONArray displayOptions = jsonObject.getJSONArray(DISPLAY_OPTIONS_KEY);
		Map<String, Boolean> selected = readDisplayProperties(displayOptions);
		JSONObject monthProperties = jsonObject.getJSONObject(MONTH_OPTIONS_KEY);
		LocalDate start = readStartMonthProperties(monthProperties);
		LocalDate end = readEndMonthProperties(monthProperties);
		JSONArray scenarioPaths = jsonObject.getJSONArray(SCENARIOS_KEY);
		List<EpptScenarioRun> scenarioRuns = readEpptScenarioRuns(scenarioPaths);
		String name = jsonObject.getString(NAME_KEY);
		String description = jsonObject.getString(DESCRIPTION_KEY);
		return new EpptProject(name, description, scenarioRuns, start, end, selected);
	}
}

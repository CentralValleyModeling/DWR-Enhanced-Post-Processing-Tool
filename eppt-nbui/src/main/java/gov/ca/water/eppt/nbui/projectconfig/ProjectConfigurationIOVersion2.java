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

package gov.ca.water.eppt.nbui.projectconfig;

import java.awt.Component;
import java.awt.Container;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.ZonedDateTime;
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
import gov.ca.water.calgui.project.EpptConfigurationController;
import javafx.scene.paint.Color;
import org.json.JSONArray;
import org.json.JSONObject;

import static gov.ca.water.eppt.nbui.projectconfig.ProjectConfigurationIO.*;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-06-2019
 */
class ProjectConfigurationIOVersion2
{
	private static final String RELATIVE_TO_PROJECT = "_RELATIVE_TO_PROJECT_";
	private static final String RELATIVE_TO_MODEL = "_RELATIVE_TO_MODEL_";
	private static final String RELATIVE_TO_INSTALLER = "_RELATIVE_TO_INSTALLER_";

	void saveConfiguration(Path selectedPath, EpptConfigurationController epptConfigurationController) throws IOException
	{
		try(BufferedWriter bufferedWriter = Files.newBufferedWriter(selectedPath))
		{
			JSONObject jsonObject = new JSONObject();
			jsonObject.put(VERSION_KEY, getVersion());
			jsonObject.put(NAME_KEY, epptConfigurationController.getProjectName());
			jsonObject.put(DESCRIPTION_KEY, epptConfigurationController.getProjectDescription());
			jsonObject.put(CREATION_DATE_KEY, ZonedDateTime.now());
			jsonObject.put(SCENARIOS_KEY, buildScenarioFilesArray(selectedPath, epptConfigurationController));
			jsonObject.write(bufferedWriter, 4, 4);
		}
	}

	String getVersion()
	{
		return VERSION_2_0;
	}

	private JSONArray buildScenarioFilesArray(Path selectedPath, EpptConfigurationController epptConfigurationController)
	{
		JSONArray jsonArray = new JSONArray();
		List<EpptScenarioRun> epptScenarioRuns = epptConfigurationController.getScenarioRuns();
		for(EpptScenarioRun scenario : epptScenarioRuns)
		{
			JSONObject jsonObject = new JSONObject();
			Path modelOutputPath = scenario.getOutputPath();
			jsonObject.put(SCENARIO_NAME, scenario.getName());
			jsonObject.put(SCENARIO_DESCRIPTION, scenario.getDescription());
			jsonObject.put(SCENARIO_MODEL, scenario.getModel().toString());
			jsonObject.put(SCENARIO_OUTPUT_PATH, relativizeToProject(modelOutputPath, selectedPath));
			jsonObject.put(SCENARIO_WRESL_DIR, relativizeToInstaller(scenario.getWreslDirectory()));
			jsonObject.put(SCENARIO_COLOR_KEY, Constant.colorToHex(scenario.getColor()));
			EpptDssContainer dssContainer = scenario.getDssContainer();
			JSONObject dssContainerJson = new JSONObject();
			JSONObject namedDssDvJsonObject = createDssJson(dssContainer.getDvDssFile(), modelOutputPath, selectedPath);
			JSONObject namedDssSvJsonObject = createDssJson(dssContainer.getSvDssFile(), modelOutputPath, selectedPath);
			JSONObject namedDssIvJsonObject = createDssJson(dssContainer.getIvDssFile(), modelOutputPath, selectedPath);
			JSONObject namedDssDtsJsonObject = createDssJson(dssContainer.getDtsDssFile(), modelOutputPath, selectedPath);
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
			String waterYearTable = relativizeToInstaller(scenario.getLookupDirectory());
			if(waterYearTable != null)
			{
				jsonObject.put(SCENARIO_LOOKUP_DIR, waterYearTable);
			}
			JSONArray extraDssArray = new JSONArray();
			dssContainer.getExtraDssFiles().stream()
						.map((NamedDssPath dssFile) -> createDssJson(dssFile, modelOutputPath, selectedPath))
						.filter(Objects::nonNull)
						.forEach(extraDssArray::put);
			dssContainerJson.put(SCENARIO_DSS_EXTRA, extraDssArray);
			jsonObject.put(SCENARIO_DSS_FILES, dssContainerJson);
			jsonObject.put(BASE_SELECTED_KEY, scenario.isBaseSelected());
			jsonObject.put(ALT_SELECTED_KEY, scenario.isAltSelected());
			jsonArray.put(jsonObject);
		}
		return jsonArray;
	}

	private String relativizeToProject(Path outputPath, Path projectPath)
	{
		Path parentFolder = projectPath.getParent();
		if(isChild(outputPath, parentFolder))
		{
			return Paths.get(RELATIVE_TO_PROJECT).resolve(parentFolder.relativize(outputPath)).toString();
		}
		return outputPath.toString();
	}

	private String relativizeToInstaller(Path originalPath)
	{
		if(isChild(originalPath, Paths.get("")))
		{
			return Paths.get(RELATIVE_TO_INSTALLER).resolve(Paths.get("").toAbsolutePath().relativize(originalPath)).toString();
		}
		return originalPath.toString();
	}

	private String relativizeToModel(Path originalPath, Path modelPath, Path projectPath)
	{
		if(isChild(originalPath, modelPath))
		{
			return Paths.get(RELATIVE_TO_MODEL).resolve(modelPath.toAbsolutePath().relativize(originalPath)).toString();
		}
		else
		{
			return relativizeToProject(originalPath, projectPath);
		}
	}

	private static boolean isChild(Path child, Path parent)
	{
		parent = parent.toAbsolutePath();
		return child.startsWith(parent);
	}

	private List<EpptScenarioRun> readEpptScenarioRuns(JSONArray scenarioPaths, Path projectPath)
	{
		List<EpptScenarioRun> retval = new ArrayList<>();
		for(int i = 0; i < scenarioPaths.length(); i++)
		{
			JSONObject scenarioJson = scenarioPaths.getJSONObject(i);
			String name = scenarioJson.getString(SCENARIO_NAME);
			String description = scenarioJson.getString(SCENARIO_DESCRIPTION);
			GUILinksAllModelsBO.Model model = GUILinksAllModelsBO.Model.findModel(
					scenarioJson.getString(SCENARIO_MODEL));
			Path modelOutputPath = unrelativizeFromProject(scenarioJson.getString(SCENARIO_OUTPUT_PATH), projectPath);
			Path wreslMain = readWreslDirectory(scenarioJson);
			Path waterYearTable = readLookupDirectory(scenarioJson);
			Color color = Constant.getPlotlyDefaultColor(i);
			if(scenarioJson.has(SCENARIO_COLOR_KEY))
			{
				color = Color.web(scenarioJson.getString(SCENARIO_COLOR_KEY));
			}
			JSONObject jsonObject = scenarioJson.getJSONObject(SCENARIO_DSS_FILES);
			NamedDssPath dvDssFile = null;
			if(jsonObject.has(SCENARIO_DV_KEY))
			{
				dvDssFile = readDssJson(jsonObject.getJSONObject(SCENARIO_DV_KEY), modelOutputPath, projectPath);
			}
			NamedDssPath svDssFile = null;
			if(jsonObject.has(SCENARIO_SV_KEY))
			{
				svDssFile = readDssJson(jsonObject.getJSONObject(SCENARIO_SV_KEY), modelOutputPath, projectPath);
			}
			NamedDssPath ivDssFile = null;
			if(jsonObject.has(SCENARIO_IV_KEY))
			{
				ivDssFile = readDssJson(jsonObject.getJSONObject(SCENARIO_IV_KEY), modelOutputPath, projectPath);
			}
			NamedDssPath dtsDssFile = null;
			if(jsonObject.has(SCENARIO_DTS_KEY))
			{
				dtsDssFile = readDssJson(jsonObject.getJSONObject(SCENARIO_DTS_KEY), modelOutputPath, projectPath);
			}

			List<NamedDssPath> extraDssFiles = readExtraDss(jsonObject, modelOutputPath, projectPath);
			EpptDssContainer dssContainer = new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, dtsDssFile, extraDssFiles);

			EpptScenarioRun epptScenarioRun = new EpptScenarioRun(name, description,model, modelOutputPath, wreslMain, waterYearTable, dssContainer, color);

			boolean baseSelected = false;
			if(scenarioJson.has(BASE_SELECTED_KEY))
			{
				baseSelected = scenarioJson.getBoolean(BASE_SELECTED_KEY);
			}
			boolean altSelected = false;
			if(scenarioJson.has(ALT_SELECTED_KEY))
			{
				altSelected = scenarioJson.getBoolean(ALT_SELECTED_KEY);
			}
			epptScenarioRun.setBaseSelected(baseSelected);
			if(!baseSelected)
			{
				epptScenarioRun.setAltSelected(altSelected);
			}
			retval.add(epptScenarioRun);
		}
		return retval;
	}

	Path readLookupDirectory(JSONObject scenarioJson)
	{
		Path waterYearTable = Paths.get("");
		if(scenarioJson.has(SCENARIO_WATER_TABLE))
		{
			waterYearTable = unrelativizeFromInstaller(scenarioJson.getString(SCENARIO_WATER_TABLE));
			//Backwards compatibility
			if(!waterYearTable.toFile().isDirectory())
			{
				waterYearTable = waterYearTable.getParent();
			}
		}
		return waterYearTable;
	}

	Path readWreslDirectory(JSONObject scenarioJson)
	{
		Path wreslMain = Paths.get("");
		if(scenarioJson.has(SCENARIO_WRESL_MAIN))
		{
			wreslMain = unrelativizeFromInstaller(scenarioJson.getString(SCENARIO_WRESL_MAIN));
			//Backwards compatibility
			if(!wreslMain.toFile().isDirectory())
			{
				wreslMain = wreslMain.getParent();
			}
		}
		return wreslMain;
	}

	Path unrelativizeFromInstaller(String path)
	{
		if(path.startsWith(RELATIVE_TO_INSTALLER))
		{
			return Paths.get("").toAbsolutePath().resolve(path.replace(RELATIVE_TO_INSTALLER + "\\", ""));
		}
		return Paths.get(path);
	}

	private Path unrelativizeFromProject(String path, Path projectPath)
	{
		if(path.startsWith(RELATIVE_TO_PROJECT))
		{
			return projectPath.getParent().toAbsolutePath().resolve(path.replace(RELATIVE_TO_PROJECT + "\\", "").replace(RELATIVE_TO_PROJECT, ""));
		}
		return Paths.get(path);
	}

	private Path unrelativizeFromModel(String path, Path modelOutputPath, Path projectPath)
	{
		if(path.startsWith(RELATIVE_TO_MODEL))
		{
			return modelOutputPath.toAbsolutePath().resolve(path.replace(RELATIVE_TO_MODEL + "\\", ""));
		}
		else
		{
			return unrelativizeFromProject(path, projectPath);
		}
	}

	private List<NamedDssPath> readExtraDss(JSONObject jsonObject, Path modelOutputPath, Path projectPath)
	{
		List<NamedDssPath> extraDssFiles = new ArrayList<>();
		if(jsonObject.has(SCENARIO_DSS_EXTRA))
		{
			JSONArray jsonArray = jsonObject.getJSONArray(SCENARIO_DSS_EXTRA);
			for(int j = 0; j < jsonArray.length(); j++)
			{
				extraDssFiles.add(readDssJson(jsonArray.getJSONObject(j), modelOutputPath, projectPath));
			}
		}
		return extraDssFiles;
	}

	private NamedDssPath readDssJson(JSONObject dssJson, Path modelOutputPath, Path projectPath)
	{
		String aliasName = dssJson.getString(SCENARIO_DSS_NAME);
		Path dssPath = unrelativizeFromModel(dssJson.getString(SCENARIO_DSS_PATH), modelOutputPath, projectPath);
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

	private JSONObject createDssJson(NamedDssPath dssFile, Path modelOutputPath, Path projectPath)
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
				namedDssJsonObject.put(SCENARIO_DSS_PATH, relativizeToModel(dssFile.getDssPath(), modelOutputPath, projectPath));
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

	private int readStartYearProperties(JSONObject jsonObject)
	{
		String startYear = jsonObject.getString(START_YEAR_PROPERTY);
		return Integer.parseInt(startYear);
	}

	private int readEndYearProperties(JSONObject jsonObject)
	{
		String endYear = jsonObject.getString(END_YEAR_PROPERTY);
		return Integer.parseInt(endYear);
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

	EpptProject loadConfiguration(JSONObject jsonObject, Path projectPath)
	{
		Map<String, Boolean> selected = new HashMap<>();
		if(jsonObject.has(DISPLAY_OPTIONS_KEY))
		{
			JSONArray displayOptions = jsonObject.getJSONArray(DISPLAY_OPTIONS_KEY);
			selected = readDisplayProperties(displayOptions);
		}
		int start = 1921;
		int end = 2003;
		if(jsonObject.has(YEAR_OPTIONS_KEY))
		{

			JSONObject monthProperties = jsonObject.getJSONObject(YEAR_OPTIONS_KEY);
			start = readStartYearProperties(monthProperties);
			end = readEndYearProperties(monthProperties);
		}
		JSONArray scenarioPaths = jsonObject.getJSONArray(SCENARIOS_KEY);
		List<EpptScenarioRun> scenarioRuns = readEpptScenarioRuns(scenarioPaths, projectPath);
		String name = jsonObject.getString(NAME_KEY);
		String description = jsonObject.getString(DESCRIPTION_KEY);
		return new EpptProject(name, description, scenarioRuns, start, end, selected);
	}
}

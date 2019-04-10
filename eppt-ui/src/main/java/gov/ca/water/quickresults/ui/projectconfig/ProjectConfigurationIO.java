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
	private static final String START_MONTH_PROPERTY = "Start_Month";
	private static final String START_YEAR_PROPERTY = "Start_Year";
	private static final String END_MONTH_PROPERTY = "End_Month";
	private static final String END_YEAR_PROPERTY = "End_Year";
	private static final String VERSION_KEY = "version";
	private static final String VERSION = "1.0";
	private static final String NAME_KEY = "name";
	private static final String DESCRIPTION_KEY = "description";
	private static final String CREATION_DATE_KEY = "creation_date";
	private static final String SCENARIO_FILES_KEY = "scenario_files";
	private static final String FILE_KEY = "file";
	private static final String MODEL_KEY = "model";
	private static final String SELECTED_KEY = "selected";
	private static final String DISPLAY_OPTIONS_KEY = "display_options";
	private static final String MONTH_OPTIONS_KEY = "month_options";
	private static final String ID_KEY = "id";

	void saveConfiguration(Path selectedPath, String name, String description) throws IOException
	{
		try(BufferedWriter bufferedWriter = Files.newBufferedWriter(selectedPath))
		{
			JSONObject jsonObject = new JSONObject();
			jsonObject.put(VERSION_KEY, VERSION);
			jsonObject.put(NAME_KEY, name);
			jsonObject.put(DESCRIPTION_KEY, description);
			jsonObject.put(CREATION_DATE_KEY, ZonedDateTime.now());
			jsonObject.put(SCENARIO_FILES_KEY, buildScenarioFilesArray(selectedPath));
			jsonObject.put(DISPLAY_OPTIONS_KEY, writeSelectedProperties());
			jsonObject.put(MONTH_OPTIONS_KEY, writeMonthProperties());
			jsonObject.write(bufferedWriter, 4, 4);
		}
	}

	ProjectConfigurationDescriptor loadConfiguration(Path selectedPath) throws IOException
	{
		String collect = Files.readAllLines(selectedPath).stream().collect(Collectors.joining("\n"));
		JSONObject jsonObject = new JSONObject(collect);
		JSONArray displayOptions = jsonObject.getJSONArray(DISPLAY_OPTIONS_KEY);
		readDisplayProperties(displayOptions);
		JSONObject monthProperties = jsonObject.getJSONObject(MONTH_OPTIONS_KEY);
		readMonthProperties(monthProperties);
		JSONArray scenarioPaths = jsonObject.getJSONArray(SCENARIO_FILES_KEY);
		readScenarioDssPaths(scenarioPaths, selectedPath.getParent());
		String name = jsonObject.getString(NAME_KEY);
		String description = jsonObject.getString(DESCRIPTION_KEY);
		return new ProjectConfigurationDescriptor(name, description);
	}

	private JSONArray buildScenarioFilesArray(Path selectedPath)
	{
		JSONArray jsonArray = new JSONArray();
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		List<RBListItemBO> scenarios = projectConfigurationPanel.getScenarios();
		for(RBListItemBO scenario : scenarios)
		{
			String svFilename = scenario.toString();
			Path path = Paths.get(svFilename);
			if(path.startsWith(EpptPreferences.getScenariosPaths()))
			{
				path = selectedPath.relativize(path);
			}
			JSONObject jsonObject = new JSONObject();
			jsonObject.put(FILE_KEY, path.toString());
			jsonObject.put(SELECTED_KEY, scenario.isSelected());
			jsonObject.put(MODEL_KEY, scenario.getModel().toString());
			jsonArray.put(jsonObject);
		}
		return jsonArray;
	}

	private void readScenarioDssPaths(JSONArray jsonArray, Path selectedPath)
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		List<RBListItemBO> scenarios = new ArrayList<>();
		for(int i = 0; i < jsonArray.length(); i++)
		{
			JSONObject jsonObject = jsonArray.getJSONObject(i);
			String path;
			path = jsonObject.getString(FILE_KEY);
			if(path != null)
			{
				Path dssPath = Paths.get(path);
				if(selectedPath == null)
				{
					selectedPath = Paths.get("");
				}
				if(selectedPath.resolve(dssPath).normalize().toFile().exists())
				{
					dssPath = selectedPath.resolve(dssPath).normalize().toAbsolutePath();
				}
				if(dssPath.isAbsolute())
				{
					String modelString = null;
					if(jsonObject.has(MODEL_KEY))
					{
						modelString = jsonObject.getString(MODEL_KEY);
					}
					GUILinksAllModelsBO.Model model = GUILinksAllModelsBO.Model.findModel(modelString);
					RBListItemBO rbListItemBO = new RBListItemBO(dssPath.toString(), dssPath.getFileName().toString(), model);
					boolean selected = jsonObject.getBoolean(SELECTED_KEY);
					rbListItemBO.setSelected(selected);
					scenarios.add(rbListItemBO);
				}
			}
		}
		projectConfigurationPanel.setScenarios(scenarios);
	}

	private JSONObject writeMonthProperties()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();

		JSONObject jsonObject = new JSONObject();
		jsonObject.put(START_MONTH_PROPERTY,
				String.valueOf(projectConfigurationPanel.getStartMonth().getMonth()));
		jsonObject.put(START_YEAR_PROPERTY,
				String.valueOf(projectConfigurationPanel.getStartMonth().getYearValue()));
		jsonObject.put(END_MONTH_PROPERTY, String.valueOf(projectConfigurationPanel.getEndMonth().getMonth()));
		jsonObject.put(END_YEAR_PROPERTY, String.valueOf(projectConfigurationPanel.getEndMonth().getYearValue()));

		return jsonObject;
	}

	private void readMonthProperties(JSONObject jsonObject)
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		String startMonth = jsonObject.getString(START_MONTH_PROPERTY);
		String startYear = jsonObject.getString(START_YEAR_PROPERTY);
		String endMonth = jsonObject.getString(END_MONTH_PROPERTY);
		String endYear = jsonObject.getString(END_YEAR_PROPERTY);
		Month start = new Month(Integer.parseInt(startMonth), Integer.parseInt(startYear));
		Month end = new Month(Integer.parseInt(endMonth), Integer.parseInt(endYear));
		projectConfigurationPanel.setStartMonth(start);
		projectConfigurationPanel.setEndMonth(end);
	}

	private JSONArray writeSelectedProperties()
	{
		JSONArray jsonArray = new JSONArray();
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		Component[] components = projectConfigurationPanel.getComponents();
		writeSelectedProperties(jsonArray, components);
		return jsonArray;
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

	private void readDisplayProperties(JSONArray jsonArray)
	{
		SwingEngine swingEngine = ProjectConfigurationPanel.getProjectConfigurationPanel().getSwingEngine();
		for(int i = 0; i < jsonArray.length(); i++)
		{
			JSONObject jsonObject = jsonArray.getJSONObject(i);
			if(jsonObject != null && jsonObject.getString(ID_KEY) != null)
			{
				Component component = swingEngine.find(jsonObject.getString(ID_KEY));
				boolean selected = jsonObject.getBoolean(SELECTED_KEY);
				if(component instanceof JCheckBox)
				{
					((JCheckBox) component).setSelected(selected);
				}
				else if(component instanceof JRadioButton)
				{
					((JRadioButton) component).setSelected(selected);
				}
			}
		}
	}
}

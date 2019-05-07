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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptProject;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import org.jfree.data.time.Month;
import org.json.JSONArray;
import org.json.JSONObject;
import org.swixml.SwingEngine;

import static gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationIO.*;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-06-2019
 */
class ProjectConfigurationIOVersion1
{

	private List<EpptScenarioRun> readScenarioDssPaths(JSONArray jsonArray, Path selectedPath)
	{
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		for(int i = 0; i < jsonArray.length(); i++)
		{

			JSONObject jsonObject = jsonArray.getJSONObject(i);
			final EpptScenarioRun scenarioRun = createScenarioRunFromJson(selectedPath, jsonObject);
			if(scenarioRun != null)
			{
				scenarioRuns.add(scenarioRun);
			}
		}
		return scenarioRuns;
	}

	private EpptScenarioRun createScenarioRunFromJson(Path selectedPath, JSONObject jsonObject)
	{
		EpptScenarioRun scenarioRun = null;
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
				scenarioRun = buildScenarioRun(dssPath, modelString);
			}
		}
		return scenarioRun;
	}

	private EpptScenarioRun buildScenarioRun(Path dssPath, String modelString)
	{
		String name = dssPath.getFileName().toString();
		String description = "";
		GUILinksAllModelsBO.Model model = GUILinksAllModelsBO.Model.findModel(modelString);
		Path outputPath = dssPath.getParent();
		Path wreslMain = null;
		NamedDssPath dvDssFile = new NamedDssPath(dssPath, dssPath.getFileName().toString());
		NamedDssPath svDssFile = null;
		NamedDssPath ivDssFile = null;
		List<NamedDssPath> extraDssFiles = new ArrayList<>();
		EpptDssContainer dssContainer = new EpptDssContainer(dvDssFile, svDssFile,
				ivDssFile, extraDssFiles);
		return new EpptScenarioRun(name, description,
				model, outputPath, wreslMain, dssContainer);
	}

	private Month readStartMonthProperties(JSONObject jsonObject)
	{
		String startMonth = jsonObject.getString(START_MONTH_PROPERTY);
		String startYear = jsonObject.getString(START_YEAR_PROPERTY);
		return new Month(Integer.parseInt(startMonth), Integer.parseInt(startYear));
	}

	private Month readEndMonthProperties(JSONObject jsonObject)
	{
		String endMonth = jsonObject.getString(END_MONTH_PROPERTY);
		String endYear = jsonObject.getString(END_YEAR_PROPERTY);
		return new Month(Integer.parseInt(endMonth), Integer.parseInt(endYear));
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

	EpptProject loadConfiguration(Path selectedPath, JSONObject jsonObject)
	{
		JSONArray displayOptions = jsonObject.getJSONArray(DISPLAY_OPTIONS_KEY);
		Map<String, Boolean> selected = readDisplayProperties(displayOptions);
		JSONObject monthProperties = jsonObject.getJSONObject(MONTH_OPTIONS_KEY);
		Month start = readStartMonthProperties(monthProperties);
		Month end = readEndMonthProperties(monthProperties);
		JSONArray scenarioPaths = jsonObject.getJSONArray(SCENARIO_FILES_KEY);
		List<EpptScenarioRun> scenarioRuns = readScenarioDssPaths(scenarioPaths, selectedPath.getParent());
		String name = jsonObject.getString(NAME_KEY);
		String description = jsonObject.getString(DESCRIPTION_KEY);
		return new EpptProject(name, description, scenarioRuns, start, end, selected);
	}
}

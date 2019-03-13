/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.scenarioconfig;

import java.awt.Component;
import java.awt.Container;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.constant.EpptPreferences;
import org.jfree.data.time.Month;

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
	private static final String DSS_FILE_PROPERTY_PREFIX = "Scenario_DSS_File_";

	void saveConfiguration(Path selectedPath) throws IOException
	{
		Properties properties = new Properties();
		try(BufferedWriter bufferedWriter = Files.newBufferedWriter(selectedPath, StandardOpenOption.CREATE))
		{
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			Component[] components = projectConfigurationPanel.getComponents();
			writeSelectedProperties(properties, components);
			writeMonthProperties(properties);
			writeScenarioDssPaths(properties, selectedPath);
			properties.store(bufferedWriter, "EPPT Project Configuration");
		}
	}

	private void writeScenarioDssPaths(Properties properties, Path selectedPath)
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		List<RBListItemBO> scenarios = projectConfigurationPanel.getScenarios();
		int i = 1;
		for(RBListItemBO scenario : scenarios)
		{
			String svFilename = scenario.toString();
			Path path = Paths.get(svFilename);
			if(path.startsWith(EpptPreferences.getScenariosPaths()))
			{
				path = selectedPath.relativize(path);
			}
			properties.put(DSS_FILE_PROPERTY_PREFIX + i, path.toString());
			properties.put(DSS_FILE_PROPERTY_PREFIX + i + "_Selected", Boolean.toString(scenario.isSelected()));
			i++;
		}
	}

	private void readScenarioDssPaths(Properties properties, Path selectedPath)
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		List<RBListItemBO> scenarios = new ArrayList<>();
		int i = 1;

		String path;
		do
		{
			path = properties.getProperty(DSS_FILE_PROPERTY_PREFIX + i, null);
			if(path != null)
			{
				Path dssPath = Paths.get(path);
				if(selectedPath.resolve(dssPath).normalize().toFile().exists())
				{
					dssPath = selectedPath.resolve(dssPath).normalize().toAbsolutePath();
				}
				if(dssPath.isAbsolute())
				{
					RBListItemBO rbListItemBO = new RBListItemBO(dssPath.toString(), dssPath.getFileName().toString());
					String property = properties.getProperty(DSS_FILE_PROPERTY_PREFIX + i + "_Selected");
					rbListItemBO.setSelected(Boolean.valueOf(property));
					scenarios.add(rbListItemBO);
				}
			}
			i++;
		}
		while(path != null);
		projectConfigurationPanel.setScenarios(scenarios);
	}

	private void writeMonthProperties(Properties properties)
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		properties.setProperty(START_MONTH_PROPERTY,
				String.valueOf(projectConfigurationPanel.getStartMonth().getMonth()));
		properties.setProperty(START_YEAR_PROPERTY,
				String.valueOf(projectConfigurationPanel.getStartMonth().getYearValue()));
		properties.setProperty(END_MONTH_PROPERTY, String.valueOf(projectConfigurationPanel.getEndMonth().getMonth()));
		properties.setProperty(END_YEAR_PROPERTY,
				String.valueOf(projectConfigurationPanel.getEndMonth().getYearValue()));
	}

	private void readMonthProperties(Properties properties)
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		String startMonth = properties.getProperty(START_MONTH_PROPERTY, "10");
		String startYear = properties.getProperty(START_YEAR_PROPERTY, "1921");
		String endMonth = properties.getProperty(END_MONTH_PROPERTY, "9");
		String endYear = properties.getProperty(END_YEAR_PROPERTY, "2003");
		Month start = new Month(Integer.parseInt(startMonth), Integer.parseInt(startYear));
		Month end = new Month(Integer.parseInt(endMonth), Integer.parseInt(endYear));
		projectConfigurationPanel.setStartMonth(start);
		projectConfigurationPanel.setEndMonth(end);
	}

	private void writeSelectedProperties(Properties properties, Component[] components)
	{
		for(Component component : components)
		{
			if(component instanceof JCheckBox)
			{
				properties.put(component.getName(), String.valueOf(((JCheckBox) component).isSelected()));
			}
			else if(component instanceof JRadioButton)
			{
				properties.put(component.getName(), String.valueOf(((JRadioButton) component).isSelected()));
			}
			else if(component instanceof Container)
			{
				writeSelectedProperties(properties, ((Container) component).getComponents());
			}
		}
	}

	private void readSelectedProperties(Properties properties, Component[] components)
	{
		for(Component component : components)
		{
			if(component instanceof JCheckBox)
			{
				String property = properties.getProperty(component.getName(), Boolean.FALSE.toString());
				((JCheckBox) component).setSelected(Boolean.valueOf(property));
			}
			else if(component instanceof JRadioButton)
			{
				String property = properties.getProperty(component.getName(), Boolean.FALSE.toString());
				((JRadioButton) component).setSelected(Boolean.valueOf(property));
			}
			else if(component instanceof Container)
			{
				readSelectedProperties(properties, ((Container) component).getComponents());
			}
		}
	}

	void loadConfiguration(Path selectedPath) throws IOException
	{
		Properties properties = new Properties();
		try(BufferedReader bufferedReader = Files.newBufferedReader(selectedPath))
		{
			properties.load(bufferedReader);
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			Component[] components = projectConfigurationPanel.getComponents();
			readSelectedProperties(properties, components);
			readMonthProperties(properties);
			readScenarioDssPaths(properties, selectedPath);
		}
	}
}

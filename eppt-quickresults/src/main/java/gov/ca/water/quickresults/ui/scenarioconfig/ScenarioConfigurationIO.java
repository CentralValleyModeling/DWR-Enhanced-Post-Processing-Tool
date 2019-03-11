/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.scenarioconfig;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Properties;

import org.swixml.SwingEngine;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-11-2019
 */
class ScenarioConfigurationIO
{
	private final SwingEngine _swingEngine;

	ScenarioConfigurationIO(SwingEngine swingEngine)
	{
		_swingEngine = swingEngine;
	}

	void saveConfiguration(Path selectedPath) throws IOException
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
		Properties properties = new Properties();
		try(BufferedWriter bufferedWriter = Files.newBufferedWriter(selectedPath, StandardOpenOption.CREATE))
		{
			properties.store(bufferedWriter, "EPPT Scenario Configuration");
		}
	}

	void loadConfiguration(Path selectedPath) throws IOException
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
		Properties properties = new Properties();
		try(BufferedReader bufferedReader = Files.newBufferedReader(selectedPath))
		{
			properties.load(bufferedReader);
		}
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.scenarioconfig;

import javax.swing.*;

import gov.ca.water.quickresults.ui.quickresults.QuickResultsPanelTest;
import org.apache.log4j.Logger;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
class ScenarioConfigurationPanelTest
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanelTest.class.getName());

	@Test
	void testScenarioConfigurationPanelCreation()
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
		assertFalse(scenarioConfigurationPanel.getComponents().length == 0);
	}

	@Test
	void testQuickStateDefault()
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.createScenarioConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) scenarioConfigurationPanel.getSwingEngine().find("rdbp000");
		radioButton.setSelected(true);
		String quickState = scenarioConfigurationPanel.quickState();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateComparison()
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.createScenarioConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) scenarioConfigurationPanel.getSwingEngine().find("rdbp001");
		radioButton.setSelected(true);
		String quickState = scenarioConfigurationPanel.quickState();
		assertEquals("Comp;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateDiff()
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.createScenarioConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) scenarioConfigurationPanel.getSwingEngine().find("rdbp002");
		radioButton.setSelected(true);
		String quickState = scenarioConfigurationPanel.quickState();
		assertEquals("Diff;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateTimeSeriesPlot()
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.createScenarioConfigurationPanel();
		JCheckBox repckbTimeSeriesPlot = (JCheckBox) scenarioConfigurationPanel.getSwingEngine().find(
				"RepckbTimeSeriesPlot");
		repckbTimeSeriesPlot.setSelected(true);
		String quickState = scenarioConfigurationPanel.quickState();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateBoxAndWhiskersPlot()
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.createScenarioConfigurationPanel();
		JCheckBox repckbTimeSeriesPlot = (JCheckBox) scenarioConfigurationPanel.getSwingEngine().find(
				"RepckbBAWPlot");
		repckbTimeSeriesPlot.setSelected(true);
		String quickState = scenarioConfigurationPanel.quickState();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;BP;ST-,Avg,All years", quickState);
	}
}

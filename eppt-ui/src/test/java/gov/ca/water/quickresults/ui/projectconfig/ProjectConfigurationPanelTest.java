/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.projectconfig;

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
class ProjectConfigurationPanelTest
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanelTest.class.getName());

	@Test
	void testScenarioConfigurationPanelCreation()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		assertFalse(projectConfigurationPanel.getComponents().length == 0);
	}

	@Test
	void testQuickStateDefault()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createScenarioConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) projectConfigurationPanel.getSwingEngine().find("rdbp000");
		radioButton.setSelected(true);
		String quickState = projectConfigurationPanel.quickState();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateComparison()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createScenarioConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) projectConfigurationPanel.getSwingEngine().find("rdbp001");
		radioButton.setSelected(true);
		String quickState = projectConfigurationPanel.quickState();
		assertEquals("Comp;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateDiff()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createScenarioConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) projectConfigurationPanel.getSwingEngine().find("rdbp002");
		radioButton.setSelected(true);
		String quickState = projectConfigurationPanel.quickState();
		assertEquals("Diff;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateTimeSeriesPlot()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createScenarioConfigurationPanel();
		JCheckBox repckbTimeSeriesPlot = (JCheckBox) projectConfigurationPanel.getSwingEngine().find(
				"RepckbTimeSeriesPlot");
		repckbTimeSeriesPlot.setSelected(true);
		String quickState = projectConfigurationPanel.quickState();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateBoxAndWhiskersPlot()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createScenarioConfigurationPanel();
		JCheckBox repckbTimeSeriesPlot = (JCheckBox) projectConfigurationPanel.getSwingEngine().find(
				"RepckbBAWPlot");
		repckbTimeSeriesPlot.setSelected(true);
		String quickState = projectConfigurationPanel.quickState();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;BP;ST-,Avg,All years", quickState);
	}
}

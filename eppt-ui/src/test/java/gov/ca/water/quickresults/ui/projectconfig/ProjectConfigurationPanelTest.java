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

import java.nio.file.Path;
import java.nio.file.Paths;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import org.junit.jupiter.api.BeforeAll;
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


	@BeforeAll
	static void setup() throws EpptInitializationException
	{
		Path target = Paths.get(System.getProperty("user.dir")).resolve("target").resolve("test-classes");
		System.setProperty("user.dir", target.toString());
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
	}

	@Test
	void testProjectConfigurationPanelCreation()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		assertFalse(projectConfigurationPanel.getComponents().length == 0);
	}

	@Test
	void testQuickStateDefault()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createProjectConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) projectConfigurationPanel.getSwingEngine().find("rdbp000");
		radioButton.setSelected(true);
		String quickState = projectConfigurationPanel.quickStateString();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateComparison()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createProjectConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) projectConfigurationPanel.getSwingEngine().find("rdbp001");
		radioButton.setSelected(true);
		String quickState = projectConfigurationPanel.quickStateString();
		assertEquals("Comp;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateDiff()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createProjectConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) projectConfigurationPanel.getSwingEngine().find("rdbp002");
		radioButton.setSelected(true);
		String quickState = projectConfigurationPanel.quickStateString();
		assertEquals("Diff;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateTimeSeriesPlot()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createProjectConfigurationPanel();
		JCheckBox repckbTimeSeriesPlot = (JCheckBox) projectConfigurationPanel.getSwingEngine().find(
				"RepckbTimeSeriesPlot");
		repckbTimeSeriesPlot.setSelected(true);
		String quickState = projectConfigurationPanel.quickStateString();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;ST-,Avg,All years", quickState);
	}

	@Test
	void testQuickStateBoxAndWhiskersPlot()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.createProjectConfigurationPanel();
		JCheckBox repckbTimeSeriesPlot = (JCheckBox) projectConfigurationPanel.getSwingEngine().find(
				"RepckbBAWPlot");
		repckbTimeSeriesPlot.setSelected(true);
		String quickState = projectConfigurationPanel.quickStateString();
		assertEquals("Base;TAF;Oct1921-Sep2003;TS;BP;ST-,Avg,All years", quickState);
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.projectconfig;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import org.jfree.data.time.Month;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-12-2019
 */
public class ProjectConfigurationIOTest
{
	@Test
	public void testProjectConfigurationJsonScenarioReload() throws IOException
	{
		Path path = Paths.get("Test1.eppt");
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		String baseFile = Thread.currentThread().getContextClassLoader().getResource(
				"Base.dss").getFile().substring(1);
		RBListItemBO rbListItemBO = new RBListItemBO(baseFile, "Base.dss");
		rbListItemBO.setSelected(true);
		projectConfigurationPanel.setScenarios(Collections.singletonList(rbListItemBO));
		ProjectConfigurationIO projectConfigurationIO = new ProjectConfigurationIO();
		projectConfigurationIO.saveConfiguration(path);
		projectConfigurationPanel.setScenarios(Collections.emptyList());
		assertTrue(projectConfigurationPanel.getScenarios().isEmpty());
		projectConfigurationPanel.loadProjectConfiguration(path);
		assertFalse(projectConfigurationPanel.getScenarios().isEmpty());
		assertTrue(projectConfigurationPanel.getScenarios().get(0).isSelected());
		assertEquals(Paths.get(rbListItemBO.toString()),
				Paths.get(projectConfigurationPanel.getScenarios().get(0).toString()));
		assertEquals(rbListItemBO.getLabel(), projectConfigurationPanel.getScenarios().get(0).getLabel());
	}

	@Test
	public void testProjectConfigurationJsonCheckboxReload() throws IOException
	{
		Path path = Paths.get("Test1.eppt");
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		ProjectConfigurationIO projectConfigurationIO = new ProjectConfigurationIO();
		JCheckBox repchkMonJan = (JCheckBox) projectConfigurationPanel.getSwingEngine().find("RepchkMonJan");
		repchkMonJan.setSelected(true);
		projectConfigurationIO.saveConfiguration(path);
		repchkMonJan.setSelected(false);
		projectConfigurationPanel.loadProjectConfiguration(path);
		assertTrue(repchkMonJan.isSelected());
	}

	@Test
	public void testProjectConfigurationJsonMonth() throws IOException
	{
		Path path = Paths.get("Test1.eppt");
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		ProjectConfigurationIO projectConfigurationIO = new ProjectConfigurationIO();
		Month month = new Month(4, 2000);
		projectConfigurationPanel.setEndMonth(month);
		projectConfigurationIO.saveConfiguration(path);
		projectConfigurationPanel.setEndMonth(new Month(6, 1955));
		projectConfigurationPanel.loadProjectConfiguration(path);
		Month endMonth = projectConfigurationPanel.getEndMonth();
		assertEquals(month.getMonth(), endMonth.getMonth());
		assertEquals(month.getYearValue(), endMonth.getYearValue());
	}
}

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

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import org.jfree.data.time.Month;
import org.junit.jupiter.api.BeforeAll;
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

	@BeforeAll
	static void setup() throws EpptInitializationException
	{
		Path target = Paths.get(System.getProperty("user.dir")).resolve("target").resolve("test-classes");
		System.setProperty("user.dir", target.toString());
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
	}

	//	@Test
	public void testProjectConfigurationJsonScenarioReload() throws IOException
	{
		Path path = Paths.get("Test1.eppt");
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		String baseFile = Thread.currentThread().getContextClassLoader().getResource(
				"Base.dss").getFile().substring(1);
		RBListItemBO rbListItemBO = new RBListItemBO(baseFile, "target/test-classes/Base.dss",
				GUILinksAllModelsBO.Model.findModel("CalLite"));
		rbListItemBO.setSelected(true);
		projectConfigurationPanel.setScenarios(Collections.singletonList(rbListItemBO));
		ProjectConfigurationIO projectConfigurationIO = new ProjectConfigurationIO();
		projectConfigurationIO.saveConfiguration(path, "UnitTest", "test for project configuration IO");
		projectConfigurationPanel.setScenarios(Collections.emptyList());
		assertTrue(projectConfigurationPanel.getScenarios().isEmpty());
		projectConfigurationPanel.loadProjectConfiguration(path);
		assertFalse(projectConfigurationPanel.getScenarios().isEmpty());
		assertTrue(projectConfigurationPanel.getScenarios().get(0).isSelected());
		assertEquals(Paths.get(rbListItemBO.toString()).getFileName(),
				Paths.get(projectConfigurationPanel.getScenarios().get(0).toString()));
		assertEquals(Paths.get(rbListItemBO.getLabel()), Paths.get(projectConfigurationPanel.getScenarios().get(0).getLabel()));
	}

	@Test
	public void testProjectConfigurationJsonCheckboxReload() throws IOException
	{
		Path path = Paths.get("Test1.eppt");
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		ProjectConfigurationIO projectConfigurationIO = new ProjectConfigurationIO();
		JCheckBox repchkMonJan = (JCheckBox) projectConfigurationPanel.getSwingEngine().find("RepchkMonJan");
		repchkMonJan.setSelected(true);
		projectConfigurationIO.saveConfiguration(path, "UnitTest", "test for project configuration IO");
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
		projectConfigurationIO.saveConfiguration(path, "UnitTest", "test for project configuration IO");
		projectConfigurationPanel.setEndMonth(new Month(6, 1955));
		projectConfigurationPanel.loadProjectConfiguration(path);
		Month endMonth = projectConfigurationPanel.getEndMonth();
		assertEquals(month.getMonth(), endMonth.getMonth());
		assertEquals(month.getYearValue(), endMonth.getYearValue());
	}
}

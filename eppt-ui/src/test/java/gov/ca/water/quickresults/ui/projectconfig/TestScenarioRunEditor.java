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
import java.util.Arrays;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import gov.ca.water.quickresults.ui.projectconfig.scenarioconfig.ScenarioRunEditor;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-08-2019
 */
class TestScenarioRunEditor
{

	@BeforeAll
	static void setup() throws EpptInitializationException
	{
		Path target = Paths.get(System.getProperty("user.dir")).resolve("target").resolve("test-classes");
		System.setProperty("user.dir", target.toString());
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
	}

	@Test
	void testDefaultState()
	{
		ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(new JFrame());
		EpptScenarioRun run = scenarioRunEditor.createRun();
		assertEquals("New Scenario Run", scenarioRunEditor.getTitle(), "Default title should be set");
		assertNull(run, "Default configuration is invalid, should be be able to create a run");
		assertTrue(scenarioRunEditor.isModal(), "Editor should be modal");
	}

	@Test
	void testFilledState()
	{
		ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(new JFrame());
		String name = "test";
		String description = "description";
		GUILinksAllModelsBO.Model model = GUILinksAllModelsBO.Model.findModel("CalSim2");
		Path outputPath = Paths.get("OUPUTPATH");
		Path wreslMain = Paths.get("WRESLMain");
		NamedDssPath dvDssFile = new NamedDssPath(Paths.get("DV FILE"), "TEST DV", "CALSIM", "1MON", "ABCDEFG");
		NamedDssPath svDssFile = new NamedDssPath(Paths.get("SV FILE"), "TEST SV", "CALSIM", "1MON", "ABCDEFG");
		NamedDssPath ivDssFile = new NamedDssPath(Paths.get("INIT File"), "TEST INIT", "CALSIM", "1MON", "ABCDEFG");
		List<NamedDssPath> extraDssFiles = Arrays.asList(new NamedDssPath(Paths.get("EXTRA 1"), "EXTRA1", "CALSIM", "1MON", "ABCDEFG"),
				new NamedDssPath(Paths.get("Extra 2"), "EXTRA2", "CALSIM", "1MON", "ABCDEFG"));
		EpptDssContainer dssContainer = new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, ivDssFile, extraDssFiles);
		EpptScenarioRun epptScenarioRun = new EpptScenarioRun(name, description, model, outputPath,
				wreslMain, Paths.get("table"), dssContainer);
		scenarioRunEditor.fillPanel(epptScenarioRun);
		assertEquals("Edit Scenario Run: " + epptScenarioRun.getName(), scenarioRunEditor.getTitle(),
				"Default title should be set");
		scenarioRunEditor.okPerformed(null);
		EpptScenarioRun newRun = scenarioRunEditor.createRun();
		assertNotNull(newRun, "Editor was filled, new Scenario should not be null");
		assertEquals(name, newRun.getName(), "Name should be set from the fillPanel method");
		assertEquals(description, newRun.getDescription(), "Description should be set from the fillPanel method");
		assertEquals(model, newRun.getModel(), "Model should be set with the new model name");
		assertEquals(outputPath, newRun.getOutputPath(), "Output path should be set with the new scenario path");
		assertEquals(wreslMain, newRun.getWreslMain(), "WRESL Main should be set with the new scenario path");
		EpptDssContainer newRunDssContainer = newRun.getDssContainer();
		assertEquals(dvDssFile, newRunDssContainer.getDvDssFile(), "DV FILE should be set with the new scenario path");
		assertEquals(svDssFile, newRunDssContainer.getSvDssFile(), "SV FILE should be set with the new scenario path");
		assertEquals(ivDssFile, newRunDssContainer.getIvDssFile(), "INIT FILE should be set with the new scenario path");
		assertEquals(extraDssFiles, newRunDssContainer.getExtraDssFiles(), "Extra DSS files should be equivalent");
	}
}

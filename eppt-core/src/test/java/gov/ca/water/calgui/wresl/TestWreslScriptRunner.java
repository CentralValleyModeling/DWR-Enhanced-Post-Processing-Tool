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

package gov.ca.water.calgui.wresl;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-01-2019
 */
public class TestWreslScriptRunner
{

	private static EpptScenarioRun EPPT_SCENARIO_RUN;
	private static EpptScenarioRun EPPT_SCENARIO_RUN_NOOP;

	static
	{
		String userDir = Paths.get(System.getProperty("user.dir")).resolve(
				"target/test-classes").toAbsolutePath().toString();
		System.setProperty("user.dir", userDir);
	}

	@BeforeAll
	static void setup() throws EpptInitializationException
	{
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		String name = "Test CalSim2";
		String description = "Unit test runner";
		GUILinksAllModelsBO.Model model = GUILinksAllModelsBO.Model.findModel("CalSim2");
		Path outputPath = Paths.get("");
		Path wreslMain  = new File(
				TestWreslScriptRunner.class.getClassLoader().getResource("mainControl.wresl").getFile()).toPath();
		Path dvPath = new File(
				TestWreslScriptRunner.class.getClassLoader().getResource("SampleDV_Base.dss").getFile()).toPath();
		NamedDssPath dvDssFile = new NamedDssPath(dvPath, "DV", "CALSIM", "1MON", "");
		Path ivPath = new File(
				TestWreslScriptRunner.class.getClassLoader().getResource("SampleINIT_Base.dss").getFile()).toPath();
		NamedDssPath ivDssFile = new NamedDssPath(ivPath, "INIT", "CALSIM", "1MON", "");
		Path dtsPath = Paths.get("DTS2.dss");
		NamedDssPath dtsDssFile = new NamedDssPath(dtsPath, "DTS", "CALSIM", "1MON", "");
		Path svPath = new File(
				TestWreslScriptRunner.class.getClassLoader().getResource("SampleSV_Base.dss").getFile()).toPath();
		NamedDssPath svDssFile = new NamedDssPath(svPath, "SV", "CALSIM", "1MON", "");
		List<NamedDssPath> extraDssFiles = Collections.emptyList();
		EpptDssContainer dssContainer = new EpptDssContainer(dvDssFile, svDssFile, ivDssFile, dtsDssFile, extraDssFiles);
		EPPT_SCENARIO_RUN = new EpptScenarioRun(name, description, model, outputPath,Paths.get("table"),
				wreslMain, dssContainer);
		EPPT_SCENARIO_RUN_NOOP = new EpptScenarioRun(name, description, model, outputPath, Paths.get("table"),
				wreslMain, dssContainer);
	}

	@Test
	void testWreslScriptRunnerMain() throws Exception
	{
		try
		{

			LocalDate start = LocalDate.ofYearDay(1922, 3);
			LocalDate end = LocalDate.ofYearDay(2000, 3);
			WreslScriptRunner wreslScriptRunner = new WreslScriptRunner(EPPT_SCENARIO_RUN_NOOP, new ProcessOutputConsumer()
			{
				@Override
				public void runStarted(EpptScenarioRun scenarioRun, Process process)
				{

				}

				@Override
				public void runFinished(Process process)
				{

				}
			});
			wreslScriptRunner.run(start, end);
		}
		catch(WreslScriptException ex)
		{
			fail(ex);
		}
	}

	@Test
	void testWreslScriptNoop()
	{
		try
		{
			LocalDate start = LocalDate.ofYearDay(1922, 3);
			LocalDate end = LocalDate.ofYearDay(2000, 3);
			Path write = new WreslConfigWriter(EPPT_SCENARIO_RUN_NOOP)
					.withStartDate(start)
					.withEndDate(end)
					.write();
			WreslScriptRunner.main(new String[]{"-config=\"" + write.toAbsolutePath().toString() + "\""});
		}
		catch(WreslScriptException ex)
		{
			fail(ex);
		}
	}
}

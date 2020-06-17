/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.quickresults.ui;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.ThresholdLinksSeedDataSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import gov.ca.water.calgui.project.EpptConfigurationController;
import org.junit.jupiter.api.Assertions;

import hec.heclib.dss.HecDSSFileAccess;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-14-2020
 */
public class WreslRunDialogScaffold
{
	public static void main(String[] args) throws Exception
	{
		HecDSSFileAccess.setMessageLevel(HecDSSFileAccess.MESS_LEVEL_CRITICAL);
		Path target = Paths.get(System.getProperty("user.dir")).resolve("target").resolve("test-classes");
		System.setProperty("user.dir", target.toString());
		WaterYearDefinitionSvc.createSeedDataSvcImplInstance();
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		ThresholdLinksSeedDataSvc.createSeedDataSvcImplInstance();

		try
		{
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch(ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e)
		{
			Assertions.fail(e);
		}
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		NamedDssPath namedDssPath = new NamedDssPath(Paths.get("J:\\DWR\\QA_QC\\SupportingDocs040219\\EPPTSupportingDoc040219\\SampleDSS_V1.01\\Inputs\\SampleDV_Base.dss"),
				"test", "CALSIM", "1MON", "2020D09E");
		EpptDssContainer dssContainer = new EpptDssContainer(namedDssPath, namedDssPath, namedDssPath, namedDssPath, Collections.emptyList());

		EpptScenarioRun baseRun = new EpptScenarioRun("Base", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"), Paths.get("Test.pdf"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\lookup\\"), dssContainer,
				javafx.scene.paint.Color.BLUEVIOLET);
		EpptScenarioRun altRun = new EpptScenarioRun("Alt", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"), Paths.get("Test.pdf"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\lookup\\"), dssContainer,
				javafx.scene.paint.Color.MEDIUMAQUAMARINE);
		List<EpptScenarioRun> scenarioRuns = Arrays.asList(baseRun, altRun);

		JFrame frame = new JFrame();
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() -> {
			EpptConfigurationController epptConfigurationController = new EpptConfigurationController();
			WreslRunDialog wreslRunDialog = new WreslRunDialog(frame, epptConfigurationController);
			wreslRunDialog.buildScenarioPanel(scenarioRuns);
			wreslRunDialog.setModal(true);
			wreslRunDialog.setVisible(true);
			System.exit(0);
		});
	}
}

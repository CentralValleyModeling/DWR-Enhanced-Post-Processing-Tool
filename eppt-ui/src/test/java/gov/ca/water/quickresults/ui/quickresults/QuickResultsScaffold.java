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

package gov.ca.water.quickresults.ui.quickresults;

import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.EpptScaffold;
import gov.ca.water.calgui.project.EpptConfigurationController;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class QuickResultsScaffold extends EpptScaffold
{

	public static void main(String[] args) throws EpptInitializationException
	{
		new QuickResultsScaffold().initScaffold();
	}


	private static EpptConfigurationController buildController()
	{
		NamedDssPath namedDssPath = new NamedDssPath(Paths.get("J:\\DWR\\QA_QC\\SupportingDocs040219\\EPPTSupportingDoc040219\\SampleDSS_V1.01\\Inputs\\SampleDV_Base.dss"),
				"test", "CALSIM", "1MON", "2020D09E");
		EpptDssContainer dssContainer = new EpptDssContainer(namedDssPath, namedDssPath, namedDssPath, namedDssPath, Collections.emptyList());
		EpptScenarioRun baseRun = new EpptScenarioRun("Base", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"), Paths.get("Test.pdf"), Paths.get("mainWresl.wresl"),
				Paths.get("target\\test-classes\\dwr_eppt\\wresl\\lookup\\wytypes.table"), dssContainer, javafx.scene.paint.Color.PINK);
		EpptScenarioRun altRun = new EpptScenarioRun("Alt", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"), Paths.get("Test.pdf"), Paths.get("mainWresl.wresl"),
				Paths.get(""), dssContainer, javafx.scene.paint.Color.PINK);
		EpptConfigurationController epptConfigurationController = new EpptConfigurationController();
		epptConfigurationController.setScenarioRuns(Arrays.asList(baseRun, altRun));
		return epptConfigurationController;
	}

	@Override
	protected EpptPanel buildEpptPanel()
	{
		EpptConfigurationController epptConfigurationController = buildController();
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel(epptConfigurationController);
		QuickResultsListener quickResultsListener = new QuickResultsListener(quickResultsPanel, epptConfigurationController);
		quickResultsPanel.getSwingEngine().setActionListener(quickResultsPanel, quickResultsListener);
		return quickResultsPanel;
	}
}

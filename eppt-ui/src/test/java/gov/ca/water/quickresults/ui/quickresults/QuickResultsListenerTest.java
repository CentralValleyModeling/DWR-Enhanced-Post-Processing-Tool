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

import java.nio.file.Path;
import java.nio.file.Paths;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.busservice.impl.EpptReportingMonths;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.ScriptedEpptStatistics;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearIndexReader;
import gov.ca.water.calgui.project.EpptConfigurationController;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class QuickResultsListenerTest
{

	@BeforeAll
	static void setup() throws EpptInitializationException
	{
		Path target = Paths.get(System.getProperty("user.dir")).resolve("target").resolve("test-classes");
		System.setProperty("user.dir", target.toString());
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		WaterYearDefinitionSvc.createSeedDataSvcImplInstance();
		WaterYearIndexReader.createInstance();
		ScriptedEpptStatistics.createScriptedStatistics();
		EpptReportingMonths.createTrendReportingMonthsInstance();
	}

	@Test
	public void testConstructor()
	{
		EpptConfigurationController epptConfigurationController = new EpptConfigurationController();
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel(epptConfigurationController);
		QuickResultsListener quickResultsListener = new QuickResultsListener(quickResultsPanel, epptConfigurationController);
		assertNotNull(quickResultsListener);
	}
}

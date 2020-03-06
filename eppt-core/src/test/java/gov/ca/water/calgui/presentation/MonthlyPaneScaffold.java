/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.presentation;

import java.awt.BorderLayout;
import java.nio.file.Paths;
import java.time.Month;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.busservice.impl.ScriptedEpptStatistics;
import gov.ca.water.calgui.busservice.impl.ThresholdLinksSeedDataSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.busservice.impl.WaterYearIndexAliasReader;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptDssContainer;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.NamedDssPath;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
public class MonthlyPaneScaffold
{
	static
	{
		String userDir = Paths.get(System.getProperty("user.dir")).resolve(
				"target/test-classes").toAbsolutePath().toString();
		System.setProperty("user.dir", userDir);
	}

	public final void initScaffold() throws EpptInitializationException
	{

		try
		{
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch(ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e)
		{
			fail(e);
		}
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		WaterYearDefinitionSvc.createSeedDataSvcImplInstance();
		ThresholdLinksSeedDataSvc.createSeedDataSvcImplInstance();
		WaterYearIndexAliasReader.createInstance();
		NamedDssPath baseDss = new NamedDssPath(
				Paths.get("J:\\DWR\\QA_QC\\SupportingDocs040219\\EPPTSupportingDoc040219\\SampleDSS_V1.01\\Inputs\\SampleDV_Base.dss"), "test base",
				"CALSIM", "1MON", "2020D09E");
		EpptDssContainer baseDssContainer = new EpptDssContainer(baseDss,
				baseDss,
				baseDss,
				baseDss,
				Collections.emptyList());
		NamedDssPath altDss = new NamedDssPath(
				Paths.get("J:\\DWR\\QA_QC\\SupportingDocs040219\\EPPTSupportingDoc040219\\SampleDSS_V1.01\\Inputs\\SampleDV_Base.dss"), "test alt",
				"CALSIM", "1MON", "2020D09E");
		EpptDssContainer altDssContainer = new EpptDssContainer(altDss,
				altDss,
				altDss,
				altDss,
				Collections.emptyList());

		EpptScenarioRun baseRun = new EpptScenarioRun("Base", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"),
				Paths.get("Test.pdf"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl"),
				Paths.get(
						"C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\lookup"),
				baseDssContainer, javafx.scene.paint.Color.BLUEVIOLET);
		baseRun.setBaseSelected(true);
		EpptScenarioRun altRun = new EpptScenarioRun("Alt", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"),
				Paths.get("Test.pdf"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl"),
				Paths.get(
						"C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\lookup"),
				altDssContainer, javafx.scene.paint.Color.MEDIUMAQUAMARINE);
		altRun.setAltSelected(true);
		EpptScenarioRun altRun2 = new EpptScenarioRun("Alternative 2", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"),
				Paths.get("Test.pdf"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl"),
				Paths.get(
						"C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\lookup"),
				altDssContainer, javafx.scene.paint.Color.CORNFLOWERBLUE);
		EpptScenarioRun altRun3 = new EpptScenarioRun("Alt", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"),
				Paths.get("Test.pdf"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl"),
				Paths.get(
						"C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\lookup"),
				altDssContainer, javafx.scene.paint.Color.CHARTREUSE);
		EpptScenarioRun altRun4 = new EpptScenarioRun("Alt", "desc", GUILinksAllModelsBO.Model.findModel("CalSim2"),
				Paths.get("Test.pdf"),
				Paths.get("C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl"),
				Paths.get(
						"C:\\Git\\DWR\\EPPT\\DWR-Enhanced-Post-Processing-Tool\\eppt-trend-reporting\\src\\test\\resources\\dwr_eppt\\wresl\\lookup"),
				altDssContainer, javafx.scene.paint.Color.BISQUE);
		ScriptedEpptStatistics.createScriptedStatistics();
		WaterYearTableReader waterYearTableReader = new WaterYearTableReader(baseRun.getLookupDirectory());
		List<WaterYearIndexAliasReader.WaterYearIndexAlias> read = WaterYearIndexAliasReader.getInstance().getAliases();
		List<EpptStatistic> trendStatistics = ScriptedEpptStatistics.getTrendStatistics();
		//		PlotConfigurationState plotConfigurationState = new PlotConfigurationState(PlotConfigurationState.ComparisonType.COMPARISON,
		//				true, false, false, false, Collections.emptyList(),
		//				false, true, false, Collections.singletonList(trendStatistics.get(0)),
		//				Collections.singletonList(read.get(0)), new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER));
		//		PlotConfigurationState plotConfigurationState = new PlotConfigurationState(PlotConfigurationState.ComparisonType.COMPARISON,
		//				true, false, false, false, Collections.emptyList(),
		//				false, true, false, trendStatistics,
		//				read, new WaterYearDefinition("", Month.OCTOBER, Month.SEPTEMBER));
		boolean displayTimeSeriesAll = true;
		boolean displayTimeSeriesAggregate = true;
		boolean displayExceedanceAll = true;
		boolean displayExceedanceAggregate = true;
		boolean displayBoxAndWhiskerAll = true;
		boolean displayBoxAndWhiskerAggregate = true;
		boolean displayMonthlyTable = true;
		boolean displaySummaryTable = true;
		boolean displayMonthlyLine = true;
		boolean displayBarCharts = true;
		PlotConfigurationState plotConfigurationState = new PlotConfigurationState(displayTimeSeriesAll, displayTimeSeriesAggregate,
				displayExceedanceAll, displayExceedanceAggregate, displayBoxAndWhiskerAll, displayBoxAndWhiskerAggregate,
				displayMonthlyTable, displaySummaryTable, displayMonthlyLine, displayBarCharts);

		GUILinksAllModelsBO guiLink = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance().getGuiLink("202");
		EpptConfigurationController epptConfigurationController = new EpptConfigurationController();
		WaterYearDefinition waterYearDefinition = new WaterYearDefinition("Test", Month.OCTOBER, Month.SEPTEMBER);
		epptConfigurationController.setWaterYearDefinition(waterYearDefinition);
		epptConfigurationController.setStartYear(1901);
		epptConfigurationController.setEndYear(2007);
		epptConfigurationController.setScenarioRuns(Arrays.asList(baseRun, altRun));
		epptConfigurationController.setMonthlyPeriods(Arrays.asList(new MonthPeriod("October - September", Month.OCTOBER, Month.SEPTEMBER),
				new MonthPeriod("March - September", Month.MARCH, Month.SEPTEMBER)));
		epptConfigurationController.setStatistics(
				Arrays.asList(ScriptedEpptStatistics.getTrendStatistics().get(0), ScriptedEpptStatistics.getTrendStatistics().get(1)));
		WaterYearPeriod longTermPeriod = new WaterYearPeriod("Long Term");
		WaterYearPeriod shortTermPeriod = new WaterYearPeriod("Short Term");
		List<WaterYearPeriodRange> longTermRanges = Arrays.asList(new WaterYearPeriodRange(longTermPeriod, new WaterYearType(1920, longTermPeriod), new WaterYearType(1999, longTermPeriod)));
		List<WaterYearPeriodRange> shortTermRanges = Arrays.asList(new WaterYearPeriodRange(shortTermPeriod, new WaterYearType(1950, shortTermPeriod), new WaterYearType(1960, shortTermPeriod)));
		List<WaterYearPeriodRangesFilter> annualFilters = Arrays.asList(new WaterYearPeriodRangesFilter("Long Term", longTermRanges, waterYearDefinition),
				new WaterYearPeriodRangesFilter("Short Term", shortTermRanges, waterYearDefinition));
		epptConfigurationController.setWaterYearPeriodRangesFilters(annualFilters);
		DisplayGuiLinkFrames displayGuiLinkFrames = new DisplayGuiLinkFrames(epptConfigurationController, plotConfigurationState,
				Collections.singletonList(guiLink));
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			List<JTabbedPane> jTabbedPanes = displayGuiLinkFrames.showDisplayFrames();
			DialogSvcImpl.installMainFrame(jFrame);
			jFrame.setLayout(new BorderLayout());
			JScrollPane scrollPane = new JScrollPane();
			scrollPane.setViewportView(jTabbedPanes.get(0));
			jFrame.add(scrollPane, BorderLayout.CENTER);
			jFrame.setSize(1600, 900);
			jFrame.setVisible(true);
		});
	}

	public static void main(String[] args) throws EpptInitializationException
	{
		new MonthlyPaneScaffold().initScaffold();
	}
}

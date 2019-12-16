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

package gov.ca.water.calgui.presentation;

import java.awt.BorderLayout;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.display.BoxPlotChartPanel;
import gov.ca.water.calgui.presentation.display.ChartPanel1;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import javatests.TestSupport;
import org.apache.log4j.Logger;
import vista.report.MonthlyReport;

import hec.io.TimeSeriesContainer;

import static java.util.stream.Collectors.toList;

/**
 * DisplayFrame class provides a frame for showing charts.
 *
 * @author tslawecki
 */
final class DisplayPlotlyFrames
{

	private static final Logger LOG = Logger.getLogger(DisplayPlotlyFrames.class.getName());
	private static final IErrorHandlingSvc ERROR_HANDLING_SVC = new ErrorHandlingSvcImpl();

	private DisplayPlotlyFrames()
	{
		throw new TestSupport.AssertionError("Utility class");
	}

	/**
	 * showDisplayFrames method creates a frame showing multiple charts
	 * according to parameters.
	 */
	static List<JTabbedPane> showDisplayFrames(PlotConfigurationState plotConfigurationState,
											   List<String> locationNames,
											   EpptScenarioRun baseRun,
											   List<EpptScenarioRun> alternatives,
											   LocalDate startMonth,
											   LocalDate endMonth)
	{
		List<JTabbedPane> tabbedPanes = new ArrayList<>();
		try
		{

			IDSSGrabber1Svc dssGrabber = new DSSGrabber1SvcImpl();
			dssGrabber.setIsCFS(!plotConfigurationState.isDisplayTaf());
			dssGrabber.setScenarioRuns(baseRun, alternatives);

			for(String locationName : locationNames)
			{
				if(locationName == null || locationName.isEmpty())
				{
					String message = "No Location selected.";
					ERROR_HANDLING_SVC.businessErrorHandler(message, message);
				}
				else
				{
					displayForLocation(plotConfigurationState, baseRun, startMonth, endMonth,
							tabbedPanes, dssGrabber, locationName);
				}
			}
		}
		catch(RuntimeException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display frame.";
			ERROR_HANDLING_SVC.businessErrorHandler(messageText, e);
		}
		return tabbedPanes;
	}

	private static void displayForLocation(PlotConfigurationState plotConfigurationState, EpptScenarioRun baseRun, LocalDate startMonth,
										   LocalDate endMonth, List<JTabbedPane> tabbedPanes, IDSSGrabber1Svc dssGrabber, String locationName)
	{
		dssGrabber.setLocation(locationName);
		if(dssGrabber.getPrimaryDSSName() == null)
		{
			String message = "No GUI_Links3.csv entry found for " + locationName + "/" + locationName + ".";
			ERROR_HANDLING_SVC.businessErrorHandler(message, message);
		}
		else if(dssGrabber.getPrimaryDSSName().isEmpty())
		{
			String message = "No DSS time series specified for " + locationName + "/" + locationName + ".";
			ERROR_HANDLING_SVC.businessErrorHandler(message, message);
		}
		else
		{
			dssGrabber.setDateRange(startMonth, endMonth);
			TimeSeriesContainer[] primaryResults = dssGrabber.getPrimarySeries();
			TimeSeriesContainer[] secondaryResults = dssGrabber.getSecondarySeries();
			dssGrabber.calcTAFforCFS(primaryResults, secondaryResults);
			TimeSeriesContainer[] diffResults = dssGrabber.getDifferenceSeries(primaryResults);
			TimeSeriesContainer[][] excResults = dssGrabber.getExceedanceSeries(primaryResults);
			TimeSeriesContainer[][] sexcResults = dssGrabber.getExceedanceSeries(secondaryResults);
			TimeSeriesContainer[][] dexcResults = dssGrabber.getExceedanceSeriesD(primaryResults);
			JTabbedPane tabbedpane = displayFrameForData(plotConfigurationState, baseRun, dssGrabber,
					new DisplayInput(locationName, primaryResults, secondaryResults, diffResults, excResults, sexcResults, dexcResults));

			tabbedPanes.add(tabbedpane);
		}
	}

	private static JTabbedPane displayFrameForData(PlotConfigurationState plotConfigurationState,
												   EpptScenarioRun baseRun,
												   IDSSGrabber1Svc dssGrabber,
												   DisplayInput displayInput)
	{
		JTabbedPane tabbedpane = new JTabbedPane();
		String plotTitle = dssGrabber.getPlotTitle();
		String sLabel = dssGrabber.getSLabel();
		String baseRunName = dssGrabber.getBaseRunName();
		String yLabel = dssGrabber.getYLabel();
		String originalUnits = dssGrabber.getOriginalUnits();
		Map<GUILinksAllModelsBO.Model, String> primaryDSSName = dssGrabber.getPrimaryDSSName();
		Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
		if(displayInput.getPrimaryResults() != null && displayInput.getPrimaryResults()[0] != null)
		{
			if(plotConfigurationState.isDisplaySummaryTable())
			{
				plotSummaryTable(plotConfigurationState, dssGrabber, displayInput, tabbedpane, plotTitle, sLabel, baseRunName);
			}

			if(plotConfigurationState.isDisplayMonthlyTable())
			{
				plotMonthlyTable(plotConfigurationState, dssGrabber, displayInput, tabbedpane, plotTitle, sLabel, baseRunName);
			}
			if(plotConfigurationState.isDisplayBoxAndWhiskerPlot())
			{
				plotBoxPlot(plotConfigurationState, displayInput, tabbedpane, plotTitle);
			}
			if(plotConfigurationState.isDoExceedance())
			{
				plotExceedance(plotConfigurationState, dssGrabber, displayInput, tabbedpane, plotTitle, sLabel, yLabel, originalUnits);
			}

			if(plotConfigurationState.isDisplayTimeSeriesPlot())
			{
				plotTimeSeries(plotConfigurationState, displayInput, tabbedpane, plotTitle, sLabel, yLabel, primaryDSSName);
			}
		}
		List<String> collect = missing.values()
									  .stream()
									  .flatMap(Collection::stream)
									  .collect(toList());
		if(!collect.isEmpty())
		{
			insertEmptyTab(tabbedpane, missing);
		}
		GUILinksAllModelsBO guiLinksAllModelsBO = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance()
																		 .getGuiLink(displayInput.getLocationName());
		String title = baseRun.getName() + " - " + guiLinksAllModelsBO.getPlotTitle();
		tabbedpane.setName(title);
		return tabbedpane;
	}

	private static void plotTimeSeries(PlotConfigurationState plotConfigurationState, DisplayInput displayInput, JTabbedPane tabbedpane,
									   String plotTitle, String sLabel, String yLabel, Map<GUILinksAllModelsBO.Model, String> primaryDSSName)
	{
		if(displayInput.getLocationName().contains(Constant.SCHEMATIC_PREFIX)
				&& !primaryDSSName.isEmpty())
		{
			List<String> titles = new ArrayList<>();
			for(Map.Entry<GUILinksAllModelsBO.Model, String> entry : primaryDSSName.entrySet())
			{
				titles.add(entry.toString() + " (" + entry.getKey() + ")");
			}
			ChartPanel1 cp2 = new ChartPanel1(Constant.SCHEMATIC_PREFIX + plotTitle,
					yLabel, displayInput.getPrimaryResults(), displayInput.getSecondaryResults(), false,
					String.join(",", titles), false);
			// abuse slabel to pass individual dataset names
			tabbedpane.insertTab("Time Series (experimental)", null, cp2, null, 0);

		}
		else if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE)
		{
			ChartPanel1 cp2 = new ChartPanel1(plotTitle, yLabel,
					displayInput.getPrimaryResults(),
					displayInput.getSecondaryResults(), false, sLabel, true);
			tabbedpane.insertTab("Time Series", null, cp2, null, 0);

		}
		else if(displayInput.getPrimaryResults().length < 2)
		{
			JPanel panel = new JPanel();
			panel.add(new JLabel("No chart - need two or more time series."));
			tabbedpane.insertTab(
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF ? "Difference" : "Comparison",
					null, panel, null,
					0);
		}
		else
		{
			if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
			{
				ChartPanel1 cp2 = new ChartPanel1(
						plotTitle + " - Difference from " + displayInput.getPrimaryResults()[0].fileName,
						yLabel, displayInput.getDiffResults(), null, false,
						sLabel);
				tabbedpane.insertTab("Difference", null, cp2, null, 0);
			}
			else if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.COMPARISON)
			{
				ChartPanel1 cp1 = new ChartPanel1(plotTitle + " - Comparison ",
						yLabel,
						displayInput.getPrimaryResults(), displayInput.getSecondaryResults(), false,
						sLabel);
				tabbedpane.insertTab("Comparison", null, cp1, null, 0);
			}
		}
	}

	private static void plotExceedance(PlotConfigurationState plotConfigurationState, IDSSGrabber1Svc dssGrabber, DisplayInput displayInput,
									   JTabbedPane tabbedpane, String plotTitle, String sLabel, String yLabel, String originalUnits)
	{
		List<String> exceedMonths = plotConfigurationState.getSelectedExceedancePlotMonths();
		for(int m1 = 11; m1 >= 0; m1--)
		{
			if(m1 < exceedMonths.size() && displayInput.getExcResults() != null)
			{
				String monthName = exceedMonths.get(m1);
				int index = Arrays.asList(MonthlyReport.months).indexOf(monthName.toUpperCase());
				insertTabForMonth(
						plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
						plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE,
						tabbedpane,
						displayInput.getPrimaryResults(), displayInput.getExcResults(),
						displayInput.getSexcResults(), displayInput.getDexcResults(), index, monthName, plotTitle,
						yLabel,
						sLabel);
			}
		}

		if(plotConfigurationState.isAnnualFlowExceedancePlots())
		{
			plotAnnualExceedance(plotConfigurationState, dssGrabber, displayInput, tabbedpane, plotTitle, originalUnits);
		}
		if(plotConfigurationState.isPlotAllExceedancePlots())
		{
			insertPlotPanel(dssGrabber,
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE,
					displayInput.getPrimaryResults(),
					displayInput.getExcResults(),
					displayInput.getSexcResults(), displayInput.getDexcResults(), tabbedpane, " - Exceedance (all months)",
					yLabel, 13,
					plotTitle + " - Exceedance (all months)",
					"Exceedance (all)");
		}
	}

	private static void plotAnnualExceedance(PlotConfigurationState plotConfigurationState, IDSSGrabber1Svc dssGrabber, DisplayInput displayInput,
											 JTabbedPane tabbedpane, String plotTitle, String originalUnits)
	{
		if("CFS".equals(originalUnits))
		{
			insertPlotPanel(dssGrabber,
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE,
					displayInput.getPrimaryResults(), displayInput.getExcResults(),
					displayInput.getSexcResults(), displayInput.getDexcResults(), tabbedpane,
					" - Exceedance (annual total)",
					"Annual Total Volume (TAF)",
					12, plotTitle + " - Exceedance (Annual Total)",
					"Exceedance (annual total)");
		}
		else
		{
			JPanel panel = new JPanel();
			panel.add(
					new JLabel("No chart - annual totals are only calculated for flows."));
			tabbedpane.insertTab("Exceedance (Annual Total)", null, panel, null, 0);
		}
	}

	private static void plotBoxPlot(PlotConfigurationState plotConfigurationState, DisplayInput displayInput, JTabbedPane tabbedpane,
									String plotTitle)
	{
		tabbedpane.insertTab("Box Plot", null, new BoxPlotChartPanel(plotTitle, displayInput.getPrimaryResults(),
						plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE),
				null, 0);
	}

	private static void plotMonthlyTable(PlotConfigurationState plotConfigurationState, IDSSGrabber1Svc dssGrabber, DisplayInput displayInput,
										 JTabbedPane tabbedpane, String plotTitle, String sLabel, String baseRunName)
	{
		MonthlyTablePanel mtp;
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			mtp = new MonthlyTablePanel(
					plotTitle + " - Difference from " + displayInput.getPrimaryResults()[0].fileName,
					displayInput.getDiffResults(), null, dssGrabber, "");
		}
		else
		{
			mtp = new MonthlyTablePanel(plotTitle, displayInput.getPrimaryResults(),
					displayInput.getSecondaryResults(),
					dssGrabber, sLabel,
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
		}
		tabbedpane.insertTab("Monthly - " + baseRunName, null, mtp, null, 0);
	}

	private static void plotSummaryTable(PlotConfigurationState plotConfigurationState, IDSSGrabber1Svc dssGrabber, DisplayInput displayInput,
										 JTabbedPane tabbedpane, String plotTitle, String sLabel, String baseRunName)
	{
		SummaryTablePanel stp;
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			stp = new SummaryTablePanel(plotTitle + " - Difference from " + displayInput.getPrimaryResults()[0].fileName,
					displayInput.getDiffResults(), null, plotConfigurationState.getSelectedSummaryTableItems(), "", dssGrabber);
		}
		else
		{
			stp = new SummaryTablePanel(plotTitle, displayInput.getPrimaryResults(),
					displayInput.getSecondaryResults(),
					plotConfigurationState.getSelectedSummaryTableItems(), sLabel, dssGrabber,
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
		}
		tabbedpane.insertTab("Summary - " + baseRunName, null, stp, null, 0);
	}

	private static void insertEmptyTab(JTabbedPane tabbedpane, Map<GUILinksAllModelsBO.Model, List<String>> missing)
	{
		StringBuilder buffer = new StringBuilder();
		buffer.append("<html><br>Not all DSS records were found, some results may be missing:<br><br>");
		missing.forEach((key, value) -> buffer.append("Model: ").append(key).append("<br>").append(
				String.join("<br>", value)).append("<br>"));
		buffer.append("</html>");
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(new JLabel(buffer.toString()), BorderLayout.PAGE_START);
		tabbedpane.insertTab("Alert - Missing DSS records", null, panel, null, 0);
	}

	private static void insertPlotPanel(IDSSGrabber1Svc dssGrabber, boolean doDifference, boolean doBase,
										TimeSeriesContainer[] primaryResults, TimeSeriesContainer[][] excResults,
										TimeSeriesContainer[][] sexcResults, TimeSeriesContainer[][] dexcResults,
										JTabbedPane tabbedpane, String s, String s2, int i2,
										String s3, String s4)
	{
		ChartPanel1 cp3 = null;
		if(doDifference && dexcResults != null)
		{
			cp3 = new ChartPanel1(
					dssGrabber.getPlotTitle() + s
							+ " - Difference from " + primaryResults[0].fileName,
					s2, dexcResults[i2], null, true, dssGrabber.getSLabel());
		}
		else if(excResults != null)
		{
			cp3 = new ChartPanel1(
					s3,
					s2, excResults[i2],
					sexcResults == null ? null : sexcResults[i2], true, dssGrabber.getSLabel(), doBase);
		}
		if(cp3 != null)
		{
			tabbedpane.insertTab(s4, null, cp3, null, 0);
		}
	}

	private static boolean insertTabForMonth(boolean doDifference, boolean doBase, JTabbedPane tabbedpane, TimeSeriesContainer[] primaryResults,
											 TimeSeriesContainer[][] excResults, TimeSeriesContainer[][] sexcResults,
											 TimeSeriesContainer[][] dexcResults, int m1, String monthName,
											 String plotTitle, String yLabel, String sLabel)
	{

		final ChartPanel1 cp3;
		final boolean plottedOne;
		if(doDifference)
		{
			cp3 = new ChartPanel1(
					plotTitle + " - Exceedance (" + monthName + ")"
							+ " - Difference from " + primaryResults[0].fileName,
					yLabel, dexcResults[m1], null, true, sLabel);
		}
		else
		{
			cp3 = new ChartPanel1(
					plotTitle + " - Exceedance (" + monthName + ")",
					yLabel, excResults[m1],
					sexcResults == null ? null : sexcResults[m1], true, sLabel, doBase);
		}
		plottedOne = true;
		tabbedpane.insertTab("Exceedance (" + monthName + ")", null, cp3, null, 0);
		return plottedOne;
	}

	private static void insertSummaryTable(DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
										   List<String> summaryTags, JTabbedPane tabbedpane,
										   TimeSeriesContainer[] primaryResults, TimeSeriesContainer[] secondaryResults,
										   TimeSeriesContainer[] diffResults)
	{
		SummaryTablePanel stp;
		if(doDifference)
		{
			stp = new SummaryTablePanel(
					dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
					diffResults, null, summaryTags, "", dssGrabber);
		}
		else
		{
			stp = new SummaryTablePanel(dssGrabber.getPlotTitle(), primaryResults, secondaryResults,
					summaryTags, dssGrabber.getSLabel(), dssGrabber, doBase);
		}
		tabbedpane.insertTab("Summary - " + dssGrabber.getBaseRunName(), null, stp, null, 0);
	}

	private static void insertMonthlyTable(DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
										   JTabbedPane tabbedpane, TimeSeriesContainer[] primaryResults,
										   TimeSeriesContainer[] secondaryResults, TimeSeriesContainer[] diffResults)
	{
		MonthlyTablePanel mtp;
		if(doDifference)
		{
			mtp = new MonthlyTablePanel(
					dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
					diffResults, null, dssGrabber, "");
		}
		else
		{
			mtp = new MonthlyTablePanel(dssGrabber.getPlotTitle(), primaryResults, secondaryResults,
					dssGrabber, dssGrabber.getSLabel(), doBase);
		}
		tabbedpane.insertTab("Monthly - " + dssGrabber.getBaseRunName(), null, mtp, null, 0);
	}


}

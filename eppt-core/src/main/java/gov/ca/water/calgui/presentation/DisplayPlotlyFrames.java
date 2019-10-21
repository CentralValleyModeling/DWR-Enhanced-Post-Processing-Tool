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
import java.awt.HeadlessException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import javax.swing.*;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.DssGrabber2Results;
import gov.ca.water.calgui.busservice.DssGrabberResults;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.presentation.display.BoxPlotChartPanel;
import gov.ca.water.calgui.presentation.display.BoxPlotChartPanel2;
import gov.ca.water.calgui.presentation.display.ChartPanel1;
import gov.ca.water.calgui.presentation.display.ChartPanel2;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel2;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel2;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import javatests.TestSupport;
import org.apache.log4j.Logger;
import vista.report.MonthlyReport;

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
	static List<JTabbedPane> showDisplayFrames(PlotConfigurationState plotConfigurationState, List<String> locationNames, EpptScenarioRun baseRun,
											   List<EpptScenarioRun> alternatives, LocalDate startMonth, LocalDate endMonth)
	{
		List<JTabbedPane> tabbedPanes = new ArrayList<>();
		try
		{

			IDSSGrabber1Svc dssGrabber = new DSSGrabber1SvcImpl();
			dssGrabber.setIsCFS(!plotConfigurationState.isDisplayTaf());
			dssGrabber.setScenarioRuns(baseRun, alternatives);

			for(String locationName : locationNames)
			{
				JTabbedPane tabbedpane = buildQuickResultsTabbedPane(plotConfigurationState, baseRun, startMonth, endMonth, dssGrabber, locationName);
				if(tabbedpane != null)
				{
					tabbedPanes.add(tabbedpane);
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

	/**
	 * Creates a frame to display DTS/MTS variables from WRIMS GUI
	 */
	static List<JTabbedPane> showDisplayFramesWRIMS(PlotConfigurationState plotConfigurationState,
													EpptScenarioRun baseRun,
													List<EpptScenarioRun> alternatives,
													DerivedTimeSeries dts,
													MultipleTimeSeries mts,
													LocalDate startMonth, LocalDate endMonth)
	{
		List<JTabbedPane> tabbedPanes = new ArrayList<>();
		try
		{

			DSSGrabber2SvcImpl dssGrabber = new DSSGrabber2SvcImpl(dts, mts);
			boolean isCFS = !plotConfigurationState.isDisplayTaf();

			String exceedMonths = "";
			List<String> summaryTags = plotConfigurationState.getSelectedSummaryTableItems();
			String[] monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
					"Dec"};

			JTabbedPane tabbedpane = new JTabbedPane();

			dssGrabber.setIsCFS(isCFS);
			dssGrabber.setScenarioRuns(baseRun, alternatives);

			dssGrabber.setDateRange(startMonth, endMonth);

			if(mts != null)
			{
				plotMts(plotConfigurationState, alternatives, mts, dssGrabber, exceedMonths, summaryTags, monthNames, tabbedpane);
			}
			else
			{
				plotDts(plotConfigurationState, dts, dssGrabber, exceedMonths, summaryTags, monthNames, tabbedpane);
			}
			Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
			if(!missing.isEmpty())
			{
				insertEmptyTab(tabbedpane, missing);
			}
			else
			{
				tabbedpane.setName("WRIMS GUI");
				tabbedPanes.add(tabbedpane);
			}
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display frame.";
			ERROR_HANDLING_SVC.businessErrorHandler(messageText, e);
		}
		return tabbedPanes;
	}

	private static void plotDts(PlotConfigurationState plotConfigurationState, DerivedTimeSeries dts, DSSGrabber2SvcImpl dssGrabber,
								String exceedMonths, List<String> summaryTags, String[] monthNames, JTabbedPane tabbedpane)
	{
		boolean doComparison = plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.COMPARISON;
		boolean doDifference = plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF;
		boolean doTimeSeries = plotConfigurationState.isDisplayTimeSeriesPlot();
		boolean doBase = plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE;
		boolean doExceedance = plotConfigurationState.isDoExceedance();
		boolean doBoxPlot = plotConfigurationState.isDisplayBoxAndWhiskerPlot();
		boolean doMonthlyTable = plotConfigurationState.isDisplayMonthlyTable();
		boolean doSummaryTable = plotConfigurationState.isDisplaySummaryTable();

		// Handle DTS

		dssGrabber.setLocation("@@" + dts.getName());

		DssGrabberResults dssGrabberResults = new DssGrabberResults(dssGrabber);

		if(doSummaryTable)
		{
			SummaryTablePanel stp = DisplaySummaryPanel.insertSummaryTable(dssGrabber, doDifference, doBase, summaryTags, dssGrabberResults);
			tabbedpane.insertTab("Summary - " + dssGrabber.getBaseRunName(), null, stp, null, 0);
		}

		if(doMonthlyTable)
		{
			MonthlyTablePanel mtp = DisplayMonthlyPanel.buildMonthlyPanel(dssGrabber, doDifference, doBase, dssGrabberResults);
			tabbedpane.insertTab("Monthly - " + dssGrabber.getBaseRunName(), null, mtp, null, 0);
		}

		if(doBoxPlot)
		{
			BoxPlotChartPanel boxPlotChartPanel = DisplayPlotlyBoxPlot.buildBoxPlot(dssGrabber, dssGrabberResults, doBase);
			tabbedpane.insertTab("Box Plot", null, boxPlotChartPanel,
					null, 0);
		}
		if(doExceedance)
		{
			insertExceedancePlots(dssGrabber, doDifference, doBase, exceedMonths, monthNames, tabbedpane,
					dssGrabberResults);
		}

		ChartPanel1 cp1;
		ChartPanel1 cp2;

		if(doTimeSeries)
		{

			if(doBase)
			{
				cp2 = DisplayPlotlyTimeSeries.buildBaseTimeSeriesPlot(dssGrabber, dssGrabberResults, doBase);
				tabbedpane.insertTab("Time Series", null, cp2, null, 0);

			}
			else if(dssGrabberResults.getPrimaryResults().length < 2)
			{
				JPanel panel = new JPanel();
				panel.add(new JLabel("No chart - need two or more time series."));
				tabbedpane.insertTab(doDifference ? "Difference" : "Comparison", null, panel, null, 0);
			}
			else
			{
				if(doDifference)
				{
					cp2 = DisplayPlotlyTimeSeries.buildDiffTimeSeriesPlot(dssGrabber, dssGrabberResults);
					tabbedpane.insertTab("Difference", null, cp2, null, 0);
				}
				else if(doComparison)
				{
					cp1 = DisplayPlotlyTimeSeries.buildComparisonTimeSeriesPlot(dssGrabber, dssGrabberResults);
					tabbedpane.insertTab("Comparison", null, cp1, null, 0);
				}
			}
		}
	}

	private static void plotMts(PlotConfigurationState plotConfigurationState, List<EpptScenarioRun> alternatives, MultipleTimeSeries mts,
								DSSGrabber2SvcImpl dssGrabber, String exceedMonths, List<String> summaryTags, String[] monthNames,
								JTabbedPane tabbedpane)
	{
		boolean doComparison = plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.COMPARISON;
		boolean doDifference = plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF;
		boolean doTimeSeries = plotConfigurationState.isDisplayTimeSeriesPlot();
		boolean doBase = plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE;
		boolean doExceedance = plotConfigurationState.isDoExceedance();
		boolean doBoxPlot = plotConfigurationState.isDisplayBoxAndWhiskerPlot();
		boolean doMonthlyTable = plotConfigurationState.isDisplayMonthlyTable();
		boolean doSummaryTable = plotConfigurationState.isDisplaySummaryTable();
		// Handle MTS

		dssGrabber.setLocation("@@" + mts.getName());

		int n = mts.getNumberOfDataReferences();
		int s = alternatives.size();

		DssGrabber2Results dssGrabber2Results = new DssGrabber2Results(dssGrabber, n, s);

		if(doSummaryTable)
		{
			SummaryTablePanel2 stp = DisplaySummaryPanel.buildSummaryPanel(mts, dssGrabber, doDifference, doBase, summaryTags, dssGrabber2Results);
			tabbedpane.insertTab("Summary - " + dssGrabber.getBaseRunName(), null, stp, null, 0);
		}

		if(doMonthlyTable)
		{
			MonthlyTablePanel2 mtp = DisplayMonthlyPanel.buildMonthlyPanel(mts, dssGrabber, doDifference, doBase, dssGrabber2Results);
			tabbedpane.insertTab("Monthly - " + dssGrabber.getBaseRunName(), null, mtp, null, 0);
		}

		if(doBoxPlot)
		{
			BoxPlotChartPanel2 boxPlotChartPanel2 = DisplayPlotlyBoxPlot.buildBoxPlot(mts, dssGrabber, doBase, dssGrabber2Results);
			tabbedpane.insertTab("Box Plot", null, boxPlotChartPanel2, null, 0);
		}

		if(doExceedance)
		{
			insertExceedancePlots(mts, dssGrabber, doDifference, doBase, exceedMonths, monthNames, tabbedpane, dssGrabber2Results);
		}

		ChartPanel2 cp1;
		ChartPanel2 cp2;

		if(doTimeSeries)
		{

			if(doBase)
			{
				cp2 = DisplayPlotlyTimeSeries.buildBaseTimeSeriesPlot(mts, dssGrabber, doBase, dssGrabber2Results);
				tabbedpane.insertTab("Time Series", null, cp2, null, 0);

			}
			else if(dssGrabber2Results.getResults()[0].length < 2)
			{
				JPanel panel = new JPanel();
				panel.add(new JLabel("No chart - need two or more time series."));
				tabbedpane.insertTab(doDifference ? "Difference" : "Comparison", null, panel, null, 0);
			}
			else
			{
				if(doDifference)
				{
					cp2 = DisplayPlotlyTimeSeries.buildDiffTimeSeriesPlot(mts, dssGrabber, dssGrabber2Results);
					tabbedpane.insertTab("Difference", null, cp2, null, 0);
				}
				else if(doComparison)
				{
					cp1 = DisplayPlotlyTimeSeries.buildComparisonTimeSeriesPlot(mts, dssGrabber, dssGrabber2Results);
					tabbedpane.insertTab("Comparison", null, cp1, null, 0);
				}
			}
		}
	}

	private static JTabbedPane buildQuickResultsTabbedPane(PlotConfigurationState plotConfigurationState, EpptScenarioRun baseRun,
														   LocalDate startMonth,
														   LocalDate endMonth, IDSSGrabber1Svc dssGrabber, String locationName)
	{
		JTabbedPane tabbedpane = null;
		if(locationName == null || locationName.isEmpty())
		{
			String message = "No Location selected.";
			ERROR_HANDLING_SVC.businessErrorHandler(message, message);
		}
		else
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

				DssGrabberResults dssGrabberResults = new DssGrabberResults(dssGrabber);

				tabbedpane = plotQuickResults(plotConfigurationState, baseRun, dssGrabber, locationName, dssGrabberResults);

			}
		}
		return tabbedpane;
	}


	private static JTabbedPane plotQuickResults(PlotConfigurationState plotConfigurationState, EpptScenarioRun baseRun, IDSSGrabber1Svc dssGrabber,
												String locationName,
												DssGrabberResults dssGrabberResults)
	{
		JTabbedPane tabbedpane = new JTabbedPane();
		if(dssGrabberResults.getPrimaryResults() != null && dssGrabberResults.getPrimaryResults()[0] != null)
		{
			if(plotConfigurationState.isDisplaySummaryTable())
			{
				SummaryTablePanel stp = DisplaySummaryPanel.buildSummaryTablePanel(plotConfigurationState, dssGrabber, dssGrabberResults);
				tabbedpane.insertTab("Summary - " + dssGrabber.getBaseRunName(), null, stp, null, 0);
			}

			if(plotConfigurationState.isDisplayMonthlyTable())
			{
				MonthlyTablePanel mtp = DisplayMonthlyPanel.buildMonthlyPanel(plotConfigurationState, dssGrabber, dssGrabberResults);
				tabbedpane.insertTab("Monthly - " + dssGrabber.getBaseRunName(), null, mtp, null, 0);
			}

			if(plotConfigurationState.isDisplayBoxAndWhiskerPlot())
			{
				tabbedpane
						.insertTab("Box Plot", null,
								DisplayPlotlyBoxPlot.buildBoxPlot(plotConfigurationState, dssGrabber, dssGrabberResults),
								null, 0);
			}
			if(plotConfigurationState.isDoExceedance())
			{
				List<String> exceedMonths = plotConfigurationState.getSelectedExceedancePlotMonths();
				for(int m1 = 11; m1 >= 0; m1--)
				{
					if(m1 < exceedMonths.size() && dssGrabberResults.getExcResults() != null)
					{
						String monthName = exceedMonths.get(m1);
						int index = Arrays.asList(MonthlyReport.months).indexOf(monthName.toUpperCase());
						ChartPanel1 cp3 = DisplayPlotlyExceedance.buildExceedanceForMonth(
								plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
								plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE,
								dssGrabberResults, index, monthName, dssGrabber.getPlotTitle(),
								dssGrabber.getYLabel(),
								dssGrabber.getSLabel());

						tabbedpane.insertTab("Exceedance (" + monthName + ")", null, cp3, null, 0);
					}
				}

				if(plotConfigurationState.isAnnualFlowExceedancePlots())
				{
					if("CFS".equals(dssGrabber.getOriginalUnits()))
					{
						ChartPanel1 cp3 = DisplayPlotlyExceedance.buildExceedance(dssGrabber,
								plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
								plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE,
								dssGrabberResults, " - Exceedance (annual total)",
								"Annual Total Volume (TAF)",
								12, dssGrabber.getPlotTitle() + " - Exceedance (Annual Total)");
						if(cp3 != null)
						{
							tabbedpane.insertTab("Exceedance (annual total)", null, cp3, null, 0);
						}
					}
					else
					{
						JPanel panel = new JPanel();
						panel.add(
								new JLabel("No chart - annual totals are only calculated for flows."));
						tabbedpane.insertTab("Exceedance (Annual Total)", null, panel, null, 0);
					}
				}
				if(plotConfigurationState.isPlotAllExceedancePlots())
				{
					ChartPanel1 cp3 = DisplayPlotlyExceedance.buildExceedance(dssGrabber,
							plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
							plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE,
							dssGrabberResults, " - Exceedance (all months)", dssGrabber.getYLabel(), 13,
							dssGrabber.getPlotTitle() + " - Exceedance (all months)");
					if(cp3 != null)
					{
						tabbedpane.insertTab("Exceedance (all)", null, cp3, null, 0);
					}
				}
			}
			ChartPanel1 cp1;
			ChartPanel1 cp2;

			if(plotConfigurationState.isDisplayTimeSeriesPlot())
			{
				if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE)
				{
					cp2 = DisplayPlotlyTimeSeries.buildBaseTimeSeriesPanel(dssGrabber, dssGrabberResults);
					tabbedpane.insertTab("Time Series", null, cp2, null, 0);

				}
				else if(dssGrabber.getPrimarySeries().length < 2)
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
						cp2 = DisplayPlotlyTimeSeries.buildDiffTimeSeriesPanel(dssGrabber, dssGrabberResults);
						tabbedpane.insertTab("Difference", null, cp2, null, 0);
					}
					else if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.COMPARISON)
					{
						cp1 = DisplayPlotlyTimeSeries.buildComparisonTimeSeriesPanel(dssGrabber, dssGrabberResults);
						tabbedpane.insertTab("Comparison", null, cp1, null, 0);
					}
				}
			}
		}
		Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
		List<String> allMissing = missing.values()
										 .stream()
										 .flatMap(Collection::stream)
										 .collect(toList());
		if(!allMissing.isEmpty())
		{
			insertEmptyTab(tabbedpane, missing);
		}
		else
		{

			GUILinksAllModelsBO guiLinksAllModelsBO = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance()
																			 .getObjById(locationName);
			String title;
			if(guiLinksAllModelsBO != null)
			{
				title = baseRun.getName() + " - " + guiLinksAllModelsBO.getPlotTitle();
			}
			else
			{
				title = locationName;
			}
			tabbedpane.setName(title);
			return tabbedpane;
		}
		return null;
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

	private static void insertExceedancePlots(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
											  String exceedMonths, String[] monthNames, JTabbedPane tabbedpane, DssGrabber2Results dssGrabber2Results)
	{
		ChartPanel2 cp3;
		// Check if any monthly plots
		boolean plottedOne = false;
		// were
		// done

		for(int m1 = 0; m1 < 12; m1++)
		{
			if(exceedMonths.contains(monthNames[m1]))
			{
				cp3 = DisplayPlotlyExceedance.buildExceedanceForMonth(mts, dssGrabber, doDifference, doBase, monthNames, dssGrabber2Results, m1);
				plottedOne = true;
				tabbedpane.insertTab("Exceedance (" + monthNames[m1] + ")", null, cp3, null, 0);
			}
		}
		if(exceedMonths.contains("ALL") || !plottedOne)
		{
			cp3 = DisplayPlotlyExceedance.buildAllExceedance(mts, dssGrabber, doDifference, dssGrabber2Results);
			tabbedpane.insertTab("Exceedance (all)", null, cp3, null, 0);
		}
		if(exceedMonths.contains("Annual"))
		{
			if("CFS".equals(dssGrabber.getOriginalUnits()))
			{
				cp3 = DisplayPlotlyExceedance.buildAnnualExceedance(mts, dssGrabber, doDifference, doBase, dssGrabber2Results);
				tabbedpane.insertTab("Exceedance (annual total)", null, cp3, null, 0);
			}
			else
			{
				JPanel panel = new JPanel();
				panel.add(new JLabel("No chart - annual totals are only calculated for flows."));
				tabbedpane.insertTab("Exceedance (Annual Total)", null, panel, null, 0);
			}
		}
	}

	private static void insertExceedancePlots(DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
											  String exceedMonths, String[] monthNames, JTabbedPane tabbedpane,
											  DssGrabberResults dssGrabberResults)
	{

		// Check if any monthly plots
		// were
		// done
		boolean plottedOne = false;
		for(int m1 = 0; m1 < 12; m1++)
		{
			String monthName = monthNames[m1];
			if(exceedMonths.contains(monthName))
			{
				ChartPanel1 cp3 = DisplayPlotlyExceedance.buildExceedanceForMonth(doDifference, doBase, dssGrabberResults,
						m1, monthName, dssGrabber.getPlotTitle(), dssGrabber.getYLabel(), dssGrabber.getSLabel());
				tabbedpane.insertTab("Exceedance (" + monthName + ")", null, cp3, null, 0);
				plottedOne = true;
			}
		}
		if(exceedMonths.contains("ALL") || !plottedOne)
		{
			ChartPanel1 cp3 = DisplayPlotlyExceedance.buildExceedance(dssGrabber, doDifference, doBase, dssGrabberResults,
					" - Exceedance (all months)", dssGrabber.getYLabel(), 13,
					dssGrabber.getPlotTitle() + " - Exceedance (all months)");
			if(cp3 != null)
			{
				tabbedpane.insertTab("Exceedance (all)", null, cp3, null, 0);
			}
		}
		if(exceedMonths.contains("Annual"))
		{
			if("CFS".equals(dssGrabber.getOriginalUnits()))
			{
				ChartPanel1 cp3 = DisplayPlotlyExceedance.buildExceedance(dssGrabber, doDifference, doBase, dssGrabberResults,
						" - Exceedance (annual total)", "Annual Total Volume (TAF)", 12,
						dssGrabber.getPlotTitle() + " - Exceedance (Annual Total)");
				if(cp3 != null)
				{
					tabbedpane.insertTab("Exceedance (annual total)", null, cp3, null, 0);
				}
			}
			else
			{
				JPanel panel = new JPanel();
				panel.add(new JLabel("No chart - annual totals are only calculated for flows."));
				tabbedpane.insertTab("Exceedance (Annual Total)", null, panel, null, 0);
			}
		}
	}


}

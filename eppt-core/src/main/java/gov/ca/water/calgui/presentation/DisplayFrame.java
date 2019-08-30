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
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import javax.swing.*;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.constant.Constant;
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
import org.apache.log4j.Logger;

import hec.io.TimeSeriesContainer;

import static java.util.stream.Collectors.toList;

/**
 * DisplayFrame class provides a frame for showing charts.
 *
 * @author tslawecki
 */
class DisplayFrame
{

	private static final Logger LOG = Logger.getLogger(DisplayFrame.class.getName());
	private static final IErrorHandlingSvc ERROR_HANDLING_SVC = new ErrorHandlingSvcImpl();
	private static final Pattern GROUP_PATTERN = Pattern.compile("\\w\\w\\w\\d\\d\\d\\d-\\w\\w\\w\\d\\d\\d\\d");


	/**
	 * showDisplayFrames method creates a frame showing multiple charts
	 * according to parameters.
	 */
	static List<JTabbedPane> showDisplayFrames(PlotConfigurationState plotConfigurationState, List<String> locationNames, EpptScenarioRun baseRun,
											   List<EpptScenarioRun> alternatives, LocalDate startMonth,
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

						JTabbedPane tabbedpane = new JTabbedPane();

						if(primaryResults != null && primaryResults[0] != null)
						{
							if(plotConfigurationState.isDisplaySummaryTable())
							{
								SummaryTablePanel stp;
								if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
								{
									stp = new SummaryTablePanel(
											dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
											diffResults, null, plotConfigurationState.getSelectedSummaryTableItems(), "", dssGrabber);
								}
								else
								{
									stp = new SummaryTablePanel(dssGrabber.getPlotTitle(), primaryResults,
											secondaryResults,
											plotConfigurationState.getSelectedSummaryTableItems(), dssGrabber.getSLabel(), dssGrabber,
											plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
								}
								tabbedpane.insertTab("Summary - " + dssGrabber.getBaseRunName(), null, stp, null, 0);
							}

							if(plotConfigurationState.isDisplayMonthlyTable())
							{
								MonthlyTablePanel mtp;
								if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
								{
									mtp = new MonthlyTablePanel(
											dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
											diffResults, null, dssGrabber, "");
								}
								else
								{
									mtp = new MonthlyTablePanel(dssGrabber.getPlotTitle(), primaryResults,
											secondaryResults,
											dssGrabber, dssGrabber.getSLabel(),
											plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
								}
								tabbedpane.insertTab("Monthly - " + dssGrabber.getBaseRunName(), null, mtp, null, 0);
							}

							if(plotConfigurationState.isDisplayBoxAndWhiskerPlot())
							{
								tabbedpane
										.insertTab("Box Plot", null,
												new BoxPlotChartPanel(dssGrabber.getPlotTitle(),
														primaryResults,
														plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE),
												null, 0);
							}
							if(plotConfigurationState.isDoExceedance())
							{
								List<String> exceedMonths = plotConfigurationState.getSelectedExceedancePlotMonths();
								for(int m1 = 11; m1 >= 0; m1--)
								{
									if(excResults != null)
									{

										insertTabForMonth(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
												plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE, tabbedpane,
												primaryResults, excResults,
												sexcResults, dexcResults, m1, exceedMonths.get(m1), dssGrabber.getPlotTitle(),
												dssGrabber.getYLabel(),
												dssGrabber.getSLabel());
									}
								}

								if(plotConfigurationState.isAnnualFlowExceedancePlots())
								{
									if("CFS".equals(dssGrabber.getOriginalUnits()))
									{
										insertPlotPanel(dssGrabber,
												plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
												plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE,
												primaryResults, excResults,
												sexcResults, dexcResults, tabbedpane,
												" - Exceedance (annual total)",
												"Annual Total Volume (TAF)",
												12, dssGrabber.getPlotTitle() + " - Exceedance (Annual Total)",
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
								if(plotConfigurationState.isPlotAllExceedancePlots())
								{
									insertPlotPanel(dssGrabber,
											plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF,
											plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE, primaryResults,
											excResults,
											sexcResults, dexcResults, tabbedpane, " - Exceedance (all months)", dssGrabber.getYLabel(), 13,
											dssGrabber.getPlotTitle() + " - Exceedance (all months)",
											"Exceedance (all)");
								}
							}
							ChartPanel1 cp1;
							ChartPanel1 cp2;

							if(plotConfigurationState.isDisplayTimeSeriesPlot())
							{
								if(locationName.contains(Constant.SCHEMATIC_PREFIX)
										&& !dssGrabber.getPrimaryDSSName().isEmpty())
								{
									List<String> titles = new ArrayList<>();
									for(Map.Entry<GUILinksAllModelsBO.Model, String> entry : dssGrabber.getPrimaryDSSName().entrySet())
									{
										titles.add(entry.toString() + " (" + entry.getKey() + ")");
									}
									cp2 = new ChartPanel1(Constant.SCHEMATIC_PREFIX + dssGrabber.getPlotTitle(),
											dssGrabber.getYLabel(), primaryResults, secondaryResults, false,
											String.join(",", titles), false);
									// abuse slabel to pass individual dataset names
									tabbedpane.insertTab("Time Series (experimental)", null, cp2, null, 0);

								}
								else if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE)
								{
									cp2 = new ChartPanel1(dssGrabber.getPlotTitle(), dssGrabber.getYLabel(),
											primaryResults,
											secondaryResults, false, dssGrabber.getSLabel(), true);
									tabbedpane.insertTab("Time Series", null, cp2, null, 0);

								}
								else if(primaryResults.length < 2)
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
									if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE)
									{
										cp2 = new ChartPanel1(
												dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
												dssGrabber.getYLabel(), diffResults, null, false,
												dssGrabber.getSLabel());
										tabbedpane.insertTab("Difference", null, cp2, null, 0);
									}
									else if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.COMPARISON)
									{
										cp1 = new ChartPanel1(dssGrabber.getPlotTitle() + " - Comparison ",
												dssGrabber.getYLabel(),
												primaryResults, secondaryResults, false,
												dssGrabber.getSLabel());
										tabbedpane.insertTab("Comparison", null, cp1, null, 0);
									}
								}
							}
						}
						Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
						boolean showFrame = false;
						List<String> collect = missing.values()
													  .stream()
													  .flatMap(Collection::stream)
													  .collect(toList());
						if(collect.isEmpty())
						{
							showFrame = true;
						}
						else if(!dssGrabber.getStopOnMissing())
						{
							insertEmptyTab(tabbedpane, missing);
							showFrame = true;
						}
						if(showFrame)
						{

							GUILinksAllModelsBO guiLinksAllModelsBO = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance()
																							 .getObjById(locationName);
							String title = baseRun.getName() + " - " + guiLinksAllModelsBO.getPlotTitle();
							tabbedpane.setName(title);
							tabbedPanes.add(tabbedpane);
						}
					}
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


	/**
	 * Creates a frame to display DTS/MTS variables from WRIMS GUI
	 *
	 * @param plotConfigurationState
	 * @param alternatives
	 * @param dts
	 * @param mts
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
			boolean doComparison = false;
			boolean doDifference = false;
			boolean doTimeSeries = false;
			boolean doBase = false;
			boolean doExceedance = false;
			boolean doBoxPlot = false;
			boolean isCFS = false;
			boolean doMonthlyTable = false;
			boolean doSummaryTable = false;
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

				// Handle MTS

				dssGrabber.setLocation("@@" + mts.getName());

				int n = mts.getNumberOfDataReferences();
				int s = alternatives.size();

				TimeSeriesContainer[][] results = new TimeSeriesContainer[n][s];
				for(int i = 0; i < n; i++)
				{
					results[i] = dssGrabber.getMultipleTimeSeries(i);
				}

				dssGrabber.calcTAFforCFS(results);

				TimeSeriesContainer[][] diffResults = dssGrabber.getDifferenceSeriesWithMultipleTimeSeries(results);
				TimeSeriesContainer[][][] excResults = dssGrabber.getExceedanceSeriesWithMultipleTimeSeries(results);
				TimeSeriesContainer[][][] dexcResults = dssGrabber.getExceedanceSeriesDWithMultipleTimeSeries(results);

				if(doSummaryTable)
				{
					SummaryTablePanel2 stp;
					if(doDifference)
					{
						stp = new SummaryTablePanel2(
								dssGrabber.getPlotTitle() + " - Difference from " + results[0][0].fileName, diffResults,
								null, summaryTags, "", null, dssGrabber, doBase, mts);
					}
					else
					{
						stp = new SummaryTablePanel2(dssGrabber.getPlotTitle(), results, null, summaryTags,
								dssGrabber.getSLabel(), null, dssGrabber, doBase, mts);
					}
					tabbedpane.insertTab("Summary - " + dssGrabber.getBaseRunName(), null, stp, null, 0);
				}

				if(doMonthlyTable)
				{
					MonthlyTablePanel2 mtp;
					if(doDifference)
					{
						mtp = new MonthlyTablePanel2(
								dssGrabber.getPlotTitle() + " - Difference from " + results[0][0].fileName, diffResults,
								dssGrabber, "", doBase, mts);
					}
					else
					{
						mtp = new MonthlyTablePanel2(dssGrabber.getPlotTitle(), results, dssGrabber,
								dssGrabber.getSLabel(),
								doBase, mts);
					}
					tabbedpane.insertTab("Monthly - " + dssGrabber.getBaseRunName(), null, mtp, null, 0);
				}

				if(doBoxPlot)
				{
					tabbedpane.insertTab("Box Plot", null, new BoxPlotChartPanel2(dssGrabber.getPlotTitle(),
							results, doBase, mts), null, 0);
				}

				ChartPanel2 cp3;
				if(doExceedance)
				{
					// Check if any monthly plots
					boolean plottedOne = false;
					// were
					// done

					for(int m1 = 0; m1 < 12; m1++)
					{
						if(exceedMonths.contains(monthNames[m1]))
						{
							if(doDifference)
							{
								cp3 = new ChartPanel2(
										dssGrabber.getPlotTitle() + " - Exceedance (" + monthNames[m1] + ")"
												+ " - Difference from " + results[0][0].fileName,
										dssGrabber.getYLabel(), dexcResults[m1], true, doBase, mts);
							}
							else
							{
								cp3 = new ChartPanel2(
										dssGrabber.getPlotTitle() + " - Exceedance (" + monthNames[m1] + ")",
										dssGrabber.getYLabel(), excResults[m1], true, doBase, mts);
							}
							plottedOne = true;
							tabbedpane.insertTab("Exceedance (" + monthNames[m1] + ")", null, cp3, null, 0);
						}
					}
					if(exceedMonths.contains("ALL") || !plottedOne)
					{
						if(doDifference)
						{
							cp3 = new ChartPanel2(
									dssGrabber.getPlotTitle() + " - Exceedance (all months)" + " - Difference from "
											+ results[0][0].fileName,
									dssGrabber.getYLabel(), dexcResults[13], true, mts);
						}
						else
						{
							cp3 = new ChartPanel2(dssGrabber.getPlotTitle() + " - Exceedance (all months)",
									dssGrabber.getYLabel(), excResults[13], true, mts);
						}
						tabbedpane.insertTab("Exceedance (all)", null, cp3, null, 0);
					}
					if(exceedMonths.contains("Annual"))
					{
						if("CFS".equals(dssGrabber.getOriginalUnits()))
						{
							if(doDifference)
							{
								cp3 = new ChartPanel2(
										dssGrabber.getPlotTitle() + " - Exceedance (annual total)" + " - Difference from "
												+ results[0][0].fileName,
										"Annual Total Volume (TAF)", dexcResults[12], true, mts);
							}
							else

							{
								cp3 = new ChartPanel2(dssGrabber.getPlotTitle() + " - Exceedance (Annual Total)",
										"Annual Total Volume (TAF)", excResults[12], true, doBase, mts);
							}
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

				ChartPanel2 cp1;
				ChartPanel2 cp2;

				if(doTimeSeries)
				{

					if(doBase)
					{
						cp2 = new ChartPanel2(dssGrabber.getPlotTitle(), dssGrabber.getYLabel(), results, false, doBase, mts);
						tabbedpane.insertTab("Time Series", null, cp2, null, 0);

					}
					else if(results[0].length < 2)
					{
						JPanel panel = new JPanel();
						panel.add(new JLabel("No chart - need two or more time series."));
						tabbedpane.insertTab(doDifference ? "Difference" : "Comparison", null, panel, null, 0);
					}
					else
					{
						if(doDifference)
						{
							cp2 = new ChartPanel2(
									dssGrabber.getPlotTitle() + " - Difference from " + results[0][0].fileName,
									dssGrabber.getYLabel(), diffResults, false, mts);
							tabbedpane.insertTab("Difference", null, cp2, null, 0);
						}
						else if(doComparison)
						{
							cp1 = new ChartPanel2(dssGrabber.getPlotTitle() + " - Comparison ", dssGrabber.getYLabel(),
									results, false, mts);
							tabbedpane.insertTab("Comparison", null, cp1, null, 0);
						}
					}
				}

			}
			else
			{

				// Handle DTS

				dssGrabber.setLocation("@@" + dts.getName());

				TimeSeriesContainer[] primaryResults = dssGrabber.getPrimarySeries();
				TimeSeriesContainer[] secondaryResults = dssGrabber.getSecondarySeries();

				if(primaryResults != null)
				{
					dssGrabber.calcTAFforCFS(primaryResults, secondaryResults);
				}

				TimeSeriesContainer[] diffResults = dssGrabber.getDifferenceSeries(primaryResults);
				TimeSeriesContainer[][] excResults = dssGrabber.getExceedanceSeries(primaryResults);
				TimeSeriesContainer[][] sexcResults = dssGrabber.getExceedanceSeries(secondaryResults);
				TimeSeriesContainer[][] dexcResults = dssGrabber.getExceedanceSeriesD(primaryResults);

				if(doSummaryTable)
				{
					insertSummaryTable(dssGrabber, doDifference, doBase, summaryTags, tabbedpane, primaryResults,
							secondaryResults,
							diffResults);
				}

				if(doMonthlyTable)
				{
					insertMonthlyTable(dssGrabber, doDifference, doBase, tabbedpane, primaryResults, secondaryResults,
							diffResults);
				}

				if(doBoxPlot)
				{
					tabbedpane
							.insertTab("Box Plot", null,
									new BoxPlotChartPanel(dssGrabber.getPlotTitle(), primaryResults, doBase),
									null, 0);
				}
				if(doExceedance)
				{
					insertExceedancePlots(dssGrabber, doDifference, doBase, exceedMonths, monthNames, tabbedpane,
							primaryResults, excResults, sexcResults, dexcResults);
				}

				ChartPanel1 cp1;
				ChartPanel1 cp2;

				if(doTimeSeries)
				{

					if(doBase)
					{
						cp2 = new ChartPanel1(dssGrabber.getPlotTitle(), dssGrabber.getYLabel(), primaryResults,
								secondaryResults, false, dssGrabber.getSLabel(), doBase);
						tabbedpane.insertTab("Time Series", null, cp2, null, 0);

					}
					else if(primaryResults.length < 2)
					{
						JPanel panel = new JPanel();
						panel.add(new JLabel("No chart - need two or more time series."));
						tabbedpane.insertTab(doDifference ? "Difference" : "Comparison", null, panel, null, 0);
					}
					else
					{
						if(doDifference)
						{
							cp2 = new ChartPanel1(
									dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
									dssGrabber.getYLabel(), diffResults, null, false, dssGrabber.getSLabel());
							tabbedpane.insertTab("Difference", null, cp2, null, 0);
						}
						else if(doComparison)
						{
							cp1 = new ChartPanel1(dssGrabber.getPlotTitle() + " - Comparison ", dssGrabber.getYLabel(),
									primaryResults, secondaryResults, false, dssGrabber.getSLabel());
							tabbedpane.insertTab("Comparison", null, cp1, null, 0);
						}
					}
				}
			}
			Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
			boolean showFrame;
			if(missing.isEmpty())
			{
				showFrame = true;
			}
			else if(dssGrabber.getStopOnMissing())
			{
				showFrame = false;
			}
			else
			{
				insertEmptyTab(tabbedpane, missing);
				showFrame = true;
			}
			if(showFrame)
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

	private static void insertExceedancePlots(DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
											  String exceedMonths, String[] monthNames, JTabbedPane tabbedpane,
											  TimeSeriesContainer[] primaryResults,
											  TimeSeriesContainer[][] excResults, TimeSeriesContainer[][] sexcResults,
											  TimeSeriesContainer[][] dexcResults)
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
				plottedOne = insertTabForMonth(doDifference, doBase, tabbedpane, primaryResults,
						excResults, sexcResults, dexcResults,
						m1, monthName, dssGrabber.getPlotTitle(), dssGrabber.getYLabel(), dssGrabber.getSLabel());
			}
		}
		if(exceedMonths.contains("ALL") || !plottedOne)
		{
			insertPlotPanel(dssGrabber, doDifference, doBase, primaryResults, excResults, sexcResults, dexcResults,
					tabbedpane, " - Exceedance (all months)", dssGrabber.getYLabel(), 13,
					dssGrabber.getPlotTitle() + " - Exceedance (all months)", "Exceedance (all)");
		}
		if(exceedMonths.contains("Annual"))
		{
			if("CFS".equals(dssGrabber.getOriginalUnits()))
			{
				insertPlotPanel(dssGrabber, doDifference, doBase, primaryResults, excResults, sexcResults, dexcResults,
						tabbedpane, " - Exceedance (annual total)", "Annual Total Volume (TAF)", 12,
						dssGrabber.getPlotTitle() + " - Exceedance (Annual Total)", "Exceedance (annual total)");
			}
			else
			{
				JPanel panel = new JPanel();
				panel.add(new JLabel("No chart - annual totals are only calculated for flows."));
				tabbedpane.insertTab("Exceedance (Annual Total)", null, panel, null, 0);
			}
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

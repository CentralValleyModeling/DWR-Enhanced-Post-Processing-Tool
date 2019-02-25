/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.presentation;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.HeadlessException;
import java.awt.Toolkit;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.*;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.bus_service.IDSSGrabber1Svc;
import gov.ca.water.calgui.bus_service.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.bus_service.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.presentation.display.BoxPlotChartPanel;
import gov.ca.water.calgui.presentation.display.BoxPlotChartPanel2;
import gov.ca.water.calgui.presentation.display.ChartPanel1;
import gov.ca.water.calgui.presentation.display.ChartPanel2;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel2;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel2;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;
import org.jfree.data.time.Month;
import org.swixml.SwingEngine;

import hec.io.TimeSeriesContainer;

/**
 * DisplayFrame class provides a frame for showing charts.
 *
 * @author tslawecki
 */
public class DisplayFrame
{

	private static final Logger LOG = Logger.getLogger(DisplayFrame.class.getName());
	private static int displayLocationN = 0;
	private static int displayDeltaY = 20;
	private static int displayDeltaX = 200;
	private static IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	/**
	 * showDisplayFrames method creates a frame showing multiple charts
	 * according to parameters.
	 *
	 * @param displayGroup
	 * @param scenarios
	 */
	public static void showDisplayFrames(SwingEngine swingEngine, String displayGroup, List<RBListItemBO> scenarios)
	{

		try
		{
			IDSSGrabber1Svc dssGrabber = new DSSGrabber1SvcImpl(scenarios);
			boolean doComparison = false;
			boolean doDifference = false;
			boolean doTimeSeries = false;
			boolean doBase = false;
			boolean doExceedance = false;
			boolean doBoxPlot = false;
			boolean isCFS = false;
			boolean doMonthlyTable = false;
			boolean doSummaryTable = false;
			boolean isWeb = ((JTabbedPane) swingEngine.find("tabbedPane1")).getSelectedIndex() == 10;
			String exceedMonths = "";
			String summaryTags = "";
			String names = "";
			String locations = "";
			String dateRange = "";
			String filename = "";

			String[] groupParts = displayGroup.split(";");
			String[] monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
					"Dec"};

			for(final String groupPart : groupParts)
			{
				if("Base".equals(groupPart))
				{
					doBase = true;
				}
				if("Comp".equals(groupPart))
				{
					doComparison = true;
				}
				else if("Diff".equals(groupPart))
				{
					doDifference = true;
				}
				else if("TS".equals(groupPart))
				{
					doTimeSeries = true;
				}
				else if("BP".equals(groupPart))
				{
					doBoxPlot = true;
				}
				else if(groupPart.startsWith("EX-"))
				{
					doExceedance = true;
					exceedMonths = groupPart.substring(3);
				}
				else if("CFS".equals(groupPart))
				{
					isCFS = true;
				}
				else if("TAF".equals(groupPart))
				{
					isCFS = false;
				}
				else if("Monthly".equals(groupPart))
				{
					doMonthlyTable = true;
				}
				else if(groupPart.startsWith("ST-"))
				{
					doSummaryTable = true;
					summaryTags = groupPart.substring(4);
				}
				else if(groupPart.startsWith("Locs-"))
				{
					names = groupPart.substring(5);
				}
				else if(groupPart.startsWith("Index-"))
				{
					locations = groupPart.substring(6);
				}
				else if(groupPart.startsWith("File-"))
				{
					filename = groupPart.substring(5);
				}
				else
				{
					// Check to see if the groupPart parses as mmmyyyy-mmmyyy
					Pattern p = Pattern.compile("\\w\\w\\w\\d\\d\\d\\d-\\w\\w\\w\\d\\d\\d\\d");
					Matcher m = p.matcher(groupPart);
					if(m.find())
					{
						dateRange = groupPart;
					}
					else
					{
						LOG.debug("Unparsed display list component - " + groupPart);
					}
				}
			}

			dssGrabber.setIsCFS(isCFS);

			if(!filename.isEmpty())
			{
				dssGrabber.setBase(filename);
			}
			else
			{
				for(RBListItemBO item : scenarios)
				{
					if(item.isSelected())
					{
						dssGrabber.setBase(item.toString());
					}
				}
			}

			String[] locationNames = locations.split(",");
			String[] namesText = names.split(",");

			for(int i = 0; i < locationNames.length; i++)
			{

				if(isWeb)
				{
					dssGrabber.setLocationWeb(locationNames[i]);
				}
				else
				{
					dssGrabber.setLocation(locationNames[i]);
				}

				String message = null;
				if(dssGrabber.getPrimaryDSSName() == null)
				{
					message = "No GUI_Links3.csv entry found for " + namesText[i] + "/" + locationNames[i] + ".";
				}
				else if(dssGrabber.getPrimaryDSSName().isEmpty())
				{
					message = "No DSS time series specified for " + namesText[i] + "/" + locationNames[i] + ".";
				}
				if(message != null)
				{
					errorHandlingSvc.businessErrorHandler(message, message, null);
				}
				else
				{

					dssGrabber.setDateRange(dateRange);

					TimeSeriesContainer[] primaryResults = dssGrabber.getPrimarySeries(locationNames[i]);
					TimeSeriesContainer[] secondaryResults = dssGrabber.getSecondarySeries();

					dssGrabber.calcTAFforCFS(primaryResults, secondaryResults);

					TimeSeriesContainer[] diffResults = dssGrabber.getDifferenceSeries(primaryResults);
					TimeSeriesContainer[][] excResults = dssGrabber.getExceedanceSeries(primaryResults);
					TimeSeriesContainer[][] sexcResults = dssGrabber.getExceedanceSeries(secondaryResults);
					TimeSeriesContainer[][] dexcResults = dssGrabber.getExceedanceSeriesD(primaryResults);

					JTabbedPane tabbedpane = new JTabbedPane();

					if(doSummaryTable)
					{
						SummaryTablePanel stp;
						if(doDifference)
						{
							stp = new SummaryTablePanel(
									dssGrabber.getTitle() + " - Difference from " + primaryResults[0].fileName,
									diffResults, null, summaryTags, "", dssGrabber);
						}
						else
						{
							stp = new SummaryTablePanel(dssGrabber.getTitle(), primaryResults, secondaryResults,
									summaryTags, dssGrabber.getSLabel(), dssGrabber, doBase);
						}
						tabbedpane.insertTab("Summary - " + dssGrabber.getBase(), null, stp, null, 0);
					}

					if(doMonthlyTable)
					{
						MonthlyTablePanel mtp;
						if(doDifference)
						{
							mtp = new MonthlyTablePanel(
									dssGrabber.getTitle() + " - Difference from " + primaryResults[0].fileName,
									diffResults, null, dssGrabber, "");
						}
						else
						{
							mtp = new MonthlyTablePanel(dssGrabber.getTitle(), primaryResults, secondaryResults,
									dssGrabber, dssGrabber.getSLabel(), doBase);
						}
						tabbedpane.insertTab("Monthly - " + dssGrabber.getBase(), null, mtp, null, 0);
					}

					Date lower = new Date();
					JSpinner m = (JSpinner) swingEngine.find("spnStartMonth");
					JSpinner y = (JSpinner) swingEngine.find("spnStartYear");
					lower.setTime(
							(new Month(ResultUtilsBO.getResultUtilsInstance(null).monthToInt((String) m.getValue()),
									(Integer) y.getValue())).getFirstMillisecond());

					Date upper = new Date();
					m = (JSpinner) swingEngine.find("spnEndMonth");
					y = (JSpinner) swingEngine.find("spnEndYear");
					upper.setTime(
							(new Month(ResultUtilsBO.getResultUtilsInstance(null).monthToInt((String) m.getValue()),
									(Integer) y.getValue()).getLastMillisecond()));

					ChartPanel1 cp3;
					if(doBoxPlot)
					{
						tabbedpane
								.insertTab("Box Plot", null,
										new BoxPlotChartPanel(dssGrabber.getTitle(), dssGrabber.getYLabel(),
												primaryResults, null, lower, upper, dssGrabber.getSLabel(), doBase),
										null, 0);
					}
					if(doExceedance)
					{
						// Check if any monthly
						boolean plottedOne = false;
						// plots
						// were
						// done
						for(int m1 = 0; m1 < 12; m1++)
						{
							if(exceedMonths.contains(monthNames[m1]))
							{
								if(doDifference)
								{
									cp3 = new ChartPanel1(
											dssGrabber.getTitle() + " - Exceedance (" + monthNames[m1] + ")"
													+ " - Difference from " + primaryResults[0].fileName,
											dssGrabber.getYLabel(), dexcResults[m1], null, true, upper, lower,
											dssGrabber.getSLabel());
								}
								else
								{
									cp3 = new ChartPanel1(
											dssGrabber.getTitle() + " - Exceedance (" + monthNames[m1] + ")",
											dssGrabber.getYLabel(), excResults[m1],
											sexcResults == null ? null : sexcResults[m1], true, upper, lower,
											dssGrabber.getSLabel(), doBase);
								}
								plottedOne = true;
								tabbedpane.insertTab("Exceedance (" + monthNames[m1] + ")", null, cp3, null, 0);
							}
						}

						if(exceedMonths.contains("Annual"))
						{
							plottedOne = true;
							if(dssGrabber.getOriginalUnits().equals("CFS"))
							{
								if(doDifference)
								{
									cp3 = new ChartPanel1(
											dssGrabber.getTitle() + " - Exceedance (annual total)"
													+ " - Difference from " + primaryResults[0].fileName,
											"Annual Total Volume (TAF)", dexcResults[12], null, true, upper, lower,
											dssGrabber.getSLabel());
								}
								else

								{
									cp3 = new ChartPanel1(dssGrabber.getTitle() + " - Exceedance (Annual Total)",
											"Annual Total Volume (TAF)", excResults[12],
											sexcResults == null ? null : sexcResults[12], true, upper, lower,
											dssGrabber.getSLabel(), doBase);
								}
								tabbedpane.insertTab("Exceedance (annual total)", null, cp3, null, 0);
							}
							else
							{
								JPanel panel = new JPanel();
								panel.add(new JLabel("No chart - annual totals are only calculated for flows."));
								tabbedpane.insertTab("Exceedance (Annual Total)", null, panel, null, 0);
							}
							if(exceedMonths.contains("ALL") || !plottedOne)
							{
								if(doDifference)
								{
									cp3 = new ChartPanel1(
											dssGrabber.getTitle() + " - Exceedance (all months)" + " - Difference from "
													+ primaryResults[0].fileName,
											dssGrabber.getYLabel(), dexcResults[13], null, true, upper, lower,
											dssGrabber.getSLabel());
								}
								else
								{
									cp3 = new ChartPanel1(dssGrabber.getTitle() + " - Exceedance (all months)",
											dssGrabber.getYLabel(), excResults[13],
											sexcResults == null ? null : sexcResults[13], true, upper, lower,
											dssGrabber.getSLabel(), doBase);
								}
								tabbedpane.insertTab("Exceedance (all)", null, cp3, null, 0);
							}
						}
					}

					ChartPanel1 cp1;
					ChartPanel1 cp2;

					if(doTimeSeries)
					{
						if(locationNames[i].contains(Constant.SCHEMATIC_PREFIX)
								&& dssGrabber.getPrimaryDSSName().contains(","))
						{
							cp2 = new ChartPanel1(Constant.SCHEMATIC_PREFIX + dssGrabber.getTitle(),
									dssGrabber.getYLabel(), primaryResults, secondaryResults, false, upper, lower,
									dssGrabber.getPrimaryDSSName(), false);
							// abuse slabel to pass individual dataset names
							tabbedpane.insertTab("Time Series (experimental)", null, cp2, null, 0);

						}
						else if(doBase)
						{
							cp2 = new ChartPanel1(dssGrabber.getTitle(), dssGrabber.getYLabel(), primaryResults,
									secondaryResults, false, upper, lower, dssGrabber.getSLabel(), doBase);
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
										dssGrabber.getTitle() + " - Difference from " + primaryResults[0].fileName,
										dssGrabber.getYLabel(), diffResults, null, false, upper, lower,
										dssGrabber.getSLabel());
								tabbedpane.insertTab("Difference", null, cp2, null, 0);
							}
							else if(doComparison)
							{
								cp1 = new ChartPanel1(dssGrabber.getTitle() + " - Comparison ", dssGrabber.getYLabel(),
										primaryResults, secondaryResults, false, upper, lower,
										dssGrabber.getSLabel());
								tabbedpane.insertTab("Comparison", null, cp1, null, 0);
							}
						}
					}

					List<String> missing = dssGrabber.getMissingList();
					boolean showFrame = false;
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
						StringBuffer buffer = new StringBuffer();
						buffer.append("<html><br>Not all DSS records were found, some results may be missing:<br><br>");
						missing.forEach((id) -> buffer.append(id + "<br>"));
						buffer.append("</html>");
						JPanel panel = new JPanel();
						panel.setLayout(new BorderLayout());
						panel.add(new JLabel(buffer.toString()), BorderLayout.PAGE_START);
						tabbedpane.insertTab("Alert - Missing DSS records", null, panel, null, 0);
						showFrame = true;
					}
					if(showFrame)
					{
						JFrame frame = new JFrame();

						Container container = frame.getContentPane();
						container.add(tabbedpane);

						frame.pack();
						frame.setTitle("CalLite Results - " + namesText[i]);
						// CalLite icon
						java.net.URL imgURL = DisplayFrame.class.getClass().getResource("/images/CalLiteIcon.png");
						frame.setIconImage(Toolkit.getDefaultToolkit().getImage(imgURL));

						if(!(doTimeSeries || doExceedance || doMonthlyTable || doSummaryTable))
						{
							if(dssGrabber.getMissingList() == null)
							{
								container.add(new JLabel("Nothing to show!"));
							}
							else
							{

							}
						}

						else
						{
							tabbedpane.setSelectedIndex(0);
						}

						frame.setVisible(true);
						frame.setSize(980, 700);
						frame.setLocation(displayLocationPoint(swingEngine));

					}

				}
			}
			return;
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display frame.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
	}

	/**
	 * Returns coordinates for upper left corner of display frame. Coordinates
	 * move diagonally down the page then shift over 20 pixels, and eventually
	 * restart.
	 *
	 * @return
	 */
	private static java.awt.Point displayLocationPoint(SwingEngine swingEngine)
	{

		try
		{
			// Increment frame counter

			displayLocationN++;

			// Calculate number of rows and columns in grid, accounting for
			// bottom
			// and left margin and for diagonal organization

			int verticalSteps = (java.awt.Toolkit.getDefaultToolkit().getScreenSize().height - displayDeltaY - 200)
					/ displayDeltaY;
			int horizontalSteps = (java.awt.Toolkit.getDefaultToolkit().getScreenSize().width - 20 * verticalSteps)
					/ displayDeltaX;

			// If bottom right of grid is reached, start over

			if(displayLocationN >= verticalSteps * horizontalSteps)
			{
				displayLocationN = 0;
			}

			int displayLocationColumn = displayLocationN / verticalSteps;
			int displayLocationRow = displayLocationN - verticalSteps * displayLocationColumn;

			java.awt.Point p = new java.awt.Point();
			p.y = displayDeltaY * displayLocationRow;
			p.x = displayDeltaX * displayLocationColumn + p.y;

			return p;
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display frame.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}
		return null;

	}

	public static String quickState(SwingEngine swingEngine)
	{

		try
		{
			String cAdd;
			cAdd = "";
			// Base, Comparison and Difference
			JRadioButton rdb = (JRadioButton) swingEngine.find("rdbp000");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Base";
			}

			rdb = (JRadioButton) swingEngine.find("rdbp001");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Comp";
			}

			rdb = (JRadioButton) swingEngine.find("rdbp002");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Diff";
			}
			// Units
			rdb = (JRadioButton) swingEngine.find("rdbCFS");
			if(rdb.isSelected())
			{
				cAdd = cAdd + ";CFS";
			}
			else
			{
				cAdd = cAdd + ";TAF";
			}

			// Date
			JSpinner spnSM = (JSpinner) swingEngine.find("spnStartMonth");
			JSpinner spnEM = (JSpinner) swingEngine.find("spnEndMonth");
			JSpinner spnSY = (JSpinner) swingEngine.find("spnStartYear");
			JSpinner spnEY = (JSpinner) swingEngine.find("spnEndYear");
			String cDate = spnSM.getValue().toString() + spnSY.getValue().toString();
			cDate = cDate + "-" + spnEM.getValue().toString() + spnEY.getValue().toString();
			cAdd = cAdd + ";" + cDate;

			// Time Series
			JCheckBox ckb = (JCheckBox) swingEngine.find("RepckbTimeSeriesPlot");
			if(ckb.isSelected())
			{
				cAdd = cAdd + ";TS";
			}

			// Exceedance Plot
			// Modify processing to reflect deprecation of "Exceedance plot"
			// checkbox - Tad 2/23/17

			String cST = "";
			for(Component c : ((JPanel) swingEngine.find("controls2")).getComponents())
			{
				if(c instanceof JCheckBox)
				{
					cST = cST + (((JCheckBox) c).isSelected() ? "," + ((JCheckBox) c).getText() : "");
				}
			}
			if(!cST.isEmpty())
			{
				cAdd = cAdd + ";EX-" + cST;
			}

			// Boxplot
			if(((JCheckBox) swingEngine.find("RepckbBAWPlot")).isSelected())
			{
				cAdd = cAdd + ";BP";
			}
			// Monthly Table
			ckb = (JCheckBox) swingEngine.find("RepckbMonthlyTable");
			if(ckb.isSelected())
			{
				cAdd = cAdd + ";Monthly";
			}

			// Summary Table
			JPanel controls3 = (JPanel) swingEngine.find("controls3");
			Component[] components = controls3.getComponents();
			ckb = (JCheckBox) swingEngine.find("RepckbSummaryTable");
			if(ckb.isSelected())
			{
				cST = ";ST-";
				for(final Component component : components)
				{
					if(component instanceof JCheckBox)
					{
						JCheckBox c = (JCheckBox) component;
						if(c.isSelected())
						{
							String cName = c.getText();
							// TODO Need different naming convention.
							cST = cST + "," + cName;
						}
					}
				}
				cAdd = cAdd + cST;
			}

			return cAdd;
		}
		catch(Exception e)
		{
			LOG.error(e.getMessage(), e);
			String messageText = "Unable to display frame.";
			errorHandlingSvc.businessErrorHandler(messageText,
					(JFrame) SwingUtilities.windowForComponent(swingEngine.find("rdbp000")), e);
		}
		return null;
	}

	/**
	 * Creates a frame to display DTS/MTS variables from WRIMS GUI
	 *
	 * @param displayGroup
	 * @param lstScenarios
	 * @param dts
	 * @param mts
	 */
	public static void showDisplayFramesWRIMS(SwingEngine swingEngine, String displayGroup,
											  List<RBListItemBO> lstScenarios,
											  DerivedTimeSeries dts,
											  MultipleTimeSeries mts)
	{

		try
		{
			DSSGrabber2SvcImpl dssGrabber = new DSSGrabber2SvcImpl(lstScenarios, dts, mts);
			boolean doComparison = false;
			boolean doDifference = false;
			boolean doTimeSeries = false;
			boolean doBase = false;
			boolean doExceedance = false;
			boolean doBoxPlot = false;
			boolean isCFS = false;
			boolean doMonthlyTable = false;
			boolean doSummaryTable = false;
			boolean isWeb = ((JTabbedPane) swingEngine.find("tabbedPane1")).getSelectedIndex() == 10;
			String exceedMonths = "";
			String summaryTags = "";
			String names = "";
			String locations = "";
			String dateRange = "";
			String filename = "";

			String[] groupParts = displayGroup.split(";");
			String[] monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
					"Dec"};

			for(final String groupPart : groupParts)
			{
				if(groupPart.equals("Base"))
				{
					doBase = true;
				}
				if(groupPart.equals("Comp"))
				{
					doComparison = true;
				}
				else if(groupPart.equals("Diff"))
				{
					doDifference = true;
				}
				else if(groupPart.equals("TS"))
				{
					doTimeSeries = true;
				}
				else if(groupPart.equals("BP"))
				{
					doBoxPlot = true;
				}
				else if(groupPart.startsWith("EX-"))
				{
					doExceedance = true;
					exceedMonths = groupPart.substring(3);
				}
				else if(groupPart.equals("CFS"))
				{
					isCFS = true;
				}
				else if(groupPart.equals("TAF"))
				{
					isCFS = false;
				}
				else if(groupPart.equals("Monthly"))
				{
					doMonthlyTable = true;
				}
				else if(groupPart.startsWith("ST-"))
				{
					doSummaryTable = true;
					summaryTags = groupPart.substring(4);
				}
				else if(groupPart.startsWith("Locs-"))
				{
					names = groupPart.substring(5);
				}
				else if(groupPart.startsWith("Index-"))
				{
					locations = groupPart.substring(6);
				}
				else if(groupPart.startsWith("File-"))
				{
					filename = groupPart.substring(5);
				}
				else
				{
					// Check to see if the groupPart parses as mmmyyyy-mmmyyy
					Pattern p = Pattern.compile("\\w\\w\\w\\d\\d\\d\\d-\\w\\w\\w\\d\\d\\d\\d");
					Matcher m = p.matcher(groupPart);
					if(m.find())
					{
						dateRange = groupPart;
					}
					else
					{
						LOG.debug("Unparsed display list component - " + groupPart);
					}
				}
			}

			JTabbedPane tabbedpane = new JTabbedPane();

			dssGrabber.setIsCFS(isCFS);

			if(!filename.isEmpty())
			{
				dssGrabber.setBase(filename);
			}
			else
			{
				for(int i = 0; i < lstScenarios.size(); i++)
				{
					RBListItemBO item = lstScenarios.get(i);
					if(item.isSelected())
					{
						dssGrabber.setBase(item.toString());
					}
				}
			}

			dssGrabber.setDateRange(dateRange);

			Date lower = new Date();
			JSpinner m = (JSpinner) swingEngine.find("spnStartMonth");
			JSpinner y = (JSpinner) swingEngine.find("spnStartYear");
			lower.setTime((new Month(ResultUtilsBO.getResultUtilsInstance(null).monthToInt((String) m.getValue()),
					(Integer) y.getValue())).getFirstMillisecond());

			Date upper = new Date();
			m = (JSpinner) swingEngine.find("spnEndMonth");
			y = (JSpinner) swingEngine.find("spnEndYear");
			upper.setTime((new Month(ResultUtilsBO.getResultUtilsInstance(null).monthToInt((String) m.getValue()),
					(Integer) y.getValue()).getLastMillisecond()));

			if(mts != null)
			{

				// Handle MTS

				dssGrabber.setLocation("@@" + mts.getName());

				int n = mts.getNumberOfDataReferences();
				int s = lstScenarios.size();

				TimeSeriesContainer[][] results = new TimeSeriesContainer[n][s];
				for(int i = 0; i < n; i++)
				{
					results[i] = dssGrabber.getMultipleTimeSeries(i);
				}

				dssGrabber.calcTAFforCFS(results);

				TimeSeriesContainer[][] diff_Results = dssGrabber.getDifferenceSeriesWithMultipleTimeSeries(results);
				TimeSeriesContainer[][][] exc_Results = dssGrabber.getExceedanceSeriesWithMultipleTimeSeries(results);
				TimeSeriesContainer[][][] dexc_Results = dssGrabber.getExceedanceSeriesDWithMultipleTimeSeries(results);

				if(doSummaryTable)
				{
					SummaryTablePanel2 stp;
					if(doDifference)
					{
						stp = new SummaryTablePanel2(
								dssGrabber.getTitle() + " - Difference from " + results[0][0].fileName, diff_Results,
								null, summaryTags, "", null, dssGrabber, doBase, mts);
					}
					else
					{
						stp = new SummaryTablePanel2(dssGrabber.getTitle(), results, null, summaryTags,
								dssGrabber.getSLabel(), null, dssGrabber, doBase, mts);
					}
					tabbedpane.insertTab("Summary - " + dssGrabber.getBase(), null, stp, null, 0);
				}

				if(doMonthlyTable)
				{
					MonthlyTablePanel2 mtp;
					if(doDifference)
					{
						mtp = new MonthlyTablePanel2(
								dssGrabber.getTitle() + " - Difference from " + results[0][0].fileName, diff_Results,
								dssGrabber, "", doBase, mts);
					}
					else
					{
						mtp = new MonthlyTablePanel2(dssGrabber.getTitle(), results, dssGrabber, dssGrabber.getSLabel(),
								doBase, mts);
					}
					tabbedpane.insertTab("Monthly - " + dssGrabber.getBase(), null, mtp, null, 0);
				}

				if(doBoxPlot)
				{
					tabbedpane.insertTab("Box Plot", null, new BoxPlotChartPanel2(dssGrabber.getTitle(),
									dssGrabber.getYLabel(), results, null, lower, upper, dssGrabber.getSLabel(), doBase, mts),
							null, 0);
				}

				ChartPanel2 cp3;
				if(doExceedance)
				{

					boolean plottedOne = false; // Check if any monthly plots
					// were
					// done

					for(int m1 = 0; m1 < 12; m1++)
					{
						if(exceedMonths.contains(monthNames[m1]))
						{
							if(doDifference)
							{
								cp3 = new ChartPanel2(
										dssGrabber.getTitle() + " - Exceedance (" + monthNames[m1] + ")"
												+ " - Difference from " + results[0][0].fileName,
										dssGrabber.getYLabel(), dexc_Results[m1], true, upper, lower, doBase, mts);
							}
							else
							{
								cp3 = new ChartPanel2(dssGrabber.getTitle() + " - Exceedance (" + monthNames[m1] + ")",
										dssGrabber.getYLabel(), exc_Results[m1], true, upper, lower, doBase, mts);
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
									dssGrabber.getTitle() + " - Exceedance (all months)" + " - Difference from "
											+ results[0][0].fileName,
									dssGrabber.getYLabel(), dexc_Results[13], true, upper, lower, mts);
						}
						else
						{
							cp3 = new ChartPanel2(dssGrabber.getTitle() + " - Exceedance (all months)",
									dssGrabber.getYLabel(), exc_Results[13], true, upper, lower, mts);
						}
						tabbedpane.insertTab("Exceedance (all)", null, cp3, null, 0);
					}
					if(exceedMonths.contains("Annual"))
					{
						if(dssGrabber.getOriginalUnits().equals("CFS"))
						{
							if(doDifference)
							{
								cp3 = new ChartPanel2(
										dssGrabber.getTitle() + " - Exceedance (annual total)" + " - Difference from "
												+ results[0][0].fileName,
										"Annual Total Volume (TAF)", dexc_Results[12], true, upper, lower, mts);
							}
							else

							{
								cp3 = new ChartPanel2(dssGrabber.getTitle() + " - Exceedance (Annual Total)",
										"Annual Total Volume (TAF)", exc_Results[12], true, upper, lower, doBase, mts);
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
						cp2 = new ChartPanel2(dssGrabber.getTitle(), dssGrabber.getYLabel(), results, false, upper,
								lower, doBase, mts);
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
									dssGrabber.getTitle() + " - Difference from " + results[0][0].fileName,
									dssGrabber.getYLabel(), diff_Results, false, upper, lower, mts);
							tabbedpane.insertTab("Difference", null, cp2, null, 0);
						}
						else if(doComparison)
						{
							cp1 = new ChartPanel2(dssGrabber.getTitle() + " - Comparison ", dssGrabber.getYLabel(),
									results, false, upper, lower, mts);
							tabbedpane.insertTab("Comparison", null, cp1, null, 0);
						}
					}
				}

			}
			else
			{

				// Handle DTS

				dssGrabber.setLocation("@@" + dts.getName());

				TimeSeriesContainer[] primary_Results = dssGrabber.getPrimarySeries("DUMMY");
				TimeSeriesContainer[] secondary_Results = dssGrabber.getSecondarySeries();

				dssGrabber.calcTAFforCFS(primary_Results, secondary_Results);

				TimeSeriesContainer[] diff_Results = dssGrabber.getDifferenceSeries(primary_Results);
				TimeSeriesContainer[][] exc_Results = dssGrabber.getExceedanceSeries(primary_Results);
				TimeSeriesContainer[][] sexc_Results = dssGrabber.getExceedanceSeries(secondary_Results);
				TimeSeriesContainer[][] dexc_Results = dssGrabber.getExceedanceSeriesD(primary_Results);

				if(doSummaryTable)
				{
					SummaryTablePanel stp;
					if(doDifference)
					{
						stp = new SummaryTablePanel(
								dssGrabber.getTitle() + " - Difference from " + primary_Results[0].fileName,
								diff_Results, null, summaryTags, "", dssGrabber);
					}
					else
					{
						stp = new SummaryTablePanel(dssGrabber.getTitle(), primary_Results, secondary_Results,
								summaryTags, dssGrabber.getSLabel(), dssGrabber, doBase);
					}
					tabbedpane.insertTab("Summary - " + dssGrabber.getBase(), null, stp, null, 0);
				}

				if(doMonthlyTable)
				{
					MonthlyTablePanel mtp;
					if(doDifference)
					{
						mtp = new MonthlyTablePanel(
								dssGrabber.getTitle() + " - Difference from " + primary_Results[0].fileName,
								diff_Results, null, dssGrabber, "");
					}
					else
					{
						mtp = new MonthlyTablePanel(dssGrabber.getTitle(), primary_Results, secondary_Results,
								dssGrabber, dssGrabber.getSLabel(), doBase);
					}
					tabbedpane.insertTab("Monthly - " + dssGrabber.getBase(), null, mtp, null, 0);
				}

				if(doBoxPlot)
				{
					tabbedpane
							.insertTab("Box Plot", null,
									new BoxPlotChartPanel(dssGrabber.getTitle(), dssGrabber.getYLabel(),
											primary_Results, null, lower, upper, dssGrabber.getSLabel(), doBase),
									null, 0);
				}
				ChartPanel1 cp3;
				if(doExceedance)
				{
					// Check if any monthly plots
					// were
					// done
					boolean plottedOne = false;
					for(int m1 = 0; m1 < 12; m1++)
					{
						if(exceedMonths.contains(monthNames[m1]))
						{
							if(doDifference)
							{
								cp3 = new ChartPanel1(
										dssGrabber.getTitle() + " - Exceedance (" + monthNames[m1] + ")"
												+ " - Difference from " + primary_Results[0].fileName,
										dssGrabber.getYLabel(), dexc_Results[m1], null, true, upper, lower,
										dssGrabber.getSLabel());
							}
							else
							{
								cp3 = new ChartPanel1(dssGrabber.getTitle() + " - Exceedance (" + monthNames[m1] + ")",
										dssGrabber.getYLabel(), exc_Results[m1],
										sexc_Results == null ? null : sexc_Results[m1], true, upper, lower,
										dssGrabber.getSLabel(), doBase);
							}
							plottedOne = true;
							tabbedpane.insertTab("Exceedance (" + monthNames[m1] + ")", null, cp3, null, 0);
						}
					}
					if(exceedMonths.contains("ALL") || !plottedOne)
					{
						if(doDifference)
						{
							cp3 = new ChartPanel1(
									dssGrabber.getTitle() + " - Exceedance (all months)" + " - Difference from "
											+ primary_Results[0].fileName,
									dssGrabber.getYLabel(), dexc_Results[13], null, true, upper, lower,
									dssGrabber.getSLabel());
						}
						else
						{
							cp3 = new ChartPanel1(dssGrabber.getTitle() + " - Exceedance (all months)",
									dssGrabber.getYLabel(), exc_Results[13],
									sexc_Results == null ? null : sexc_Results[13], true, upper, lower,
									dssGrabber.getSLabel(), doBase);
						}
						tabbedpane.insertTab("Exceedance (all)", null, cp3, null, 0);
					}
					if(exceedMonths.contains("Annual"))
					{
						if(dssGrabber.getOriginalUnits().equals("CFS"))
						{
							if(doDifference)
							{
								cp3 = new ChartPanel1(
										dssGrabber.getTitle() + " - Exceedance (annual total)" + " - Difference from "
												+ primary_Results[0].fileName,
										"Annual Total Volume (TAF)", dexc_Results[12], null, true, upper, lower,
										dssGrabber.getSLabel());
							}
							else

							{
								cp3 = new ChartPanel1(dssGrabber.getTitle() + " - Exceedance (Annual Total)",
										"Annual Total Volume (TAF)", exc_Results[12],
										sexc_Results == null ? null : sexc_Results[12], true, upper, lower,
										dssGrabber.getSLabel(), doBase);
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

				ChartPanel1 cp1;
				ChartPanel1 cp2;

				if(doTimeSeries)
				{

					if(doBase)
					{
						cp2 = new ChartPanel1(dssGrabber.getTitle(), dssGrabber.getYLabel(), primary_Results,
								secondary_Results, false, upper, lower, dssGrabber.getSLabel(), doBase);
						tabbedpane.insertTab("Time Series", null, cp2, null, 0);

					}
					else if(primary_Results.length < 2)
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
									dssGrabber.getTitle() + " - Difference from " + primary_Results[0].fileName,
									dssGrabber.getYLabel(), diff_Results, null, false, upper, lower,
									dssGrabber.getSLabel());
							tabbedpane.insertTab("Difference", null, cp2, null, 0);
						}
						else if(doComparison)
						{
							cp1 = new ChartPanel1(dssGrabber.getTitle() + " - Comparison ", dssGrabber.getYLabel(),
									primary_Results, secondary_Results, false, upper, lower, dssGrabber.getSLabel());
							tabbedpane.insertTab("Comparison", null, cp1, null, 0);
						}
					}
				}
			}
			List<String> missing = dssGrabber.getMissingList();
			boolean showFrame = false;
			if(missing.size() == 0)
			{
				showFrame = true;
			}
			else if(dssGrabber.getStopOnMissing())
			{
				showFrame = false;
			}
			else
			{
				StringBuffer buffer = new StringBuffer();
				buffer.append("<html><br>Not all DSS records were found, some results may be missing:<br><br>");
				missing.forEach((id) -> buffer.append(id + "<br>"));
				buffer.append("</html>");
				JPanel panel = new JPanel();
				panel.setLayout(new BorderLayout());
				panel.add(new JLabel(buffer.toString()), BorderLayout.PAGE_START);
				tabbedpane.insertTab("Alert - Missing DSS records", null, panel, null, 0);
				showFrame = true;
			}
			if(showFrame)
			{
				JFrame frame = new JFrame();

				Container container = frame.getContentPane();
				container.add(tabbedpane);

				frame.pack();
				frame.setTitle("CalLite Results - WRIMS GUI");
				// CalLite icon
				java.net.URL imgURL = DisplayFrame.class.getClass().getResource("/images/CalLiteIcon.png");
				frame.setIconImage(Toolkit.getDefaultToolkit().getImage(imgURL));

				if(!(doTimeSeries || doExceedance || doMonthlyTable || doSummaryTable))
				{
					if(dssGrabber.getMissingList() == null)
					{
						container.add(new JLabel("Nothing to show!"));
					}
					else
					{

					}
				}

				else
				{
					tabbedpane.setSelectedIndex(0);
				}

				frame.setVisible(true);
				frame.setSize(980, 700);
				frame.setLocation(displayLocationPoint(swingEngine));
			}
			return;
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display frame.";
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) swingEngine.find(Constant.MAIN_FRAME_NAME), e);
		}

	}
}

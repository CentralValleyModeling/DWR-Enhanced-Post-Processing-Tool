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
import java.time.LocalDate;
import java.time.Month;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import javax.swing.*;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.presentation.display.BoxPlotChartPanel2;
import gov.ca.water.calgui.presentation.display.ChartPanel1;
import gov.ca.water.calgui.presentation.display.ChartPanel2;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel2;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel2;
import gov.ca.water.calgui.presentation.plotly.PlotlyPane;
import gov.ca.water.calgui.presentation.plotly.PlotlyPaneBuilder;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-20-2020
 */
public class DisplayPlotlyWrimsFrames extends DisplayFrames
{

	private static final Logger LOG = Logger.getLogger(DisplayGuiLinkFrames.class.getName());
	private static final IErrorHandlingSvc ERROR_HANDLING_SVC = new ErrorHandlingSvcImpl();
	private final DerivedTimeSeries _dts;
	private final MultipleTimeSeries _mts;

	DisplayPlotlyWrimsFrames(PlotConfigurationState plotConfigurationState,
							 DerivedTimeSeries dts,
							 MultipleTimeSeries mts,
							 EpptScenarioRun baseRun,
							 List<EpptScenarioRun> alternatives,
							 LocalDate start,
							 LocalDate end) throws EpptInitializationException
	{
		super(plotConfigurationState, baseRun, alternatives, start, end);
		_dts = dts;
		_mts = mts;
	}

	List<JTabbedPane> showDisplayFrames()
	{
		List<JTabbedPane> tabbedPanes = new ArrayList<>();

		DSSGrabber2SvcImpl dssGrabber = new DSSGrabber2SvcImpl(_dts, _mts);
		boolean doComparison = getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.COMPARISON;
		boolean doDifference = getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF;
		boolean doTimeSeries = getPlotConfigurationState().isDisplayTimeSeriesPlot();
		boolean doBase = getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.BASE;
		boolean doExceedance = getPlotConfigurationState().isDoExceedance();
		boolean doBoxPlot = getPlotConfigurationState().isDisplayBoxAndWhiskerPlot();
		boolean isCFS = !getPlotConfigurationState().isDisplayTaf();
		boolean doMonthlyTable = getPlotConfigurationState().isDisplayMonthlyTable();
		boolean doSummaryTable = getPlotConfigurationState().isDisplaySummaryTable();
		String exceedMonths = "";
		List<String> summaryTags = getPlotConfigurationState().getSelectedSummaryTableItems();
		String[] monthNames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
				"Dec"};

		JTabbedPane tabbedPane = new JTabbedPane();

		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(getBaseRun(), getAlternatives());

		dssGrabber.setDateRange(getStart(), getEnd());

		if(_mts != null)
		{
			plotMts(getAlternatives(), _mts, dssGrabber, doComparison, doDifference, doTimeSeries, doBase, doExceedance, doBoxPlot, doMonthlyTable,
					doSummaryTable,
					exceedMonths, summaryTags, monthNames, tabbedPane);

		}
		if(_dts != null)
		{
			plotDts(_dts, dssGrabber, tabbedPane);

		}
		Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
		if(!missing.isEmpty())
		{
			insertEmptyTab(tabbedPane, missing);
		}
		tabbedPane.setName("WRIMS GUI");
		tabbedPanes.add(tabbedPane);
		return tabbedPanes;
	}

	private void plotMts(List<EpptScenarioRun> alternatives, MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, boolean doComparison,
						 boolean doDifference, boolean doTimeSeries, boolean doBase, boolean doExceedance, boolean doBoxPlot,
						 boolean doMonthlyTable, boolean doSummaryTable, String exceedMonths, List<String> summaryTags, String[] monthNames,
						 JTabbedPane tabbedPane)
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
			tabbedPane.insertTab("Summary - " + dssGrabber.getBaseRunName(), null, stp, null, 0);
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
			tabbedPane.insertTab("Monthly - " + dssGrabber.getBaseRunName(), null, mtp, null, 0);
		}

		if(doBoxPlot)
		{
			tabbedPane.insertTab("Box Plot", null, new BoxPlotChartPanel2(dssGrabber.getPlotTitle(),
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
					tabbedPane.insertTab("Exceedance (" + monthNames[m1] + ")", null, cp3, null, 0);
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
				tabbedPane.insertTab("Exceedance (all)", null, cp3, null, 0);
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
					tabbedPane.insertTab("Exceedance (annual total)", null, cp3, null, 0);
				}
				else
				{
					JPanel panel = new JPanel();
					panel.add(new JLabel("No chart - annual totals are only calculated for flows."));
					tabbedPane.insertTab("Exceedance (Annual Total)", null, panel, null, 0);
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
				tabbedPane.insertTab("Time Series", null, cp2, null, 0);

			}
			else if(results[0].length < 2)
			{
				JPanel panel = new JPanel();
				panel.add(new JLabel("No chart - need two or more time series."));
				tabbedPane.insertTab(doDifference ? "Difference" : "Comparison", null, panel, null, 0);
			}
			else
			{
				if(doDifference)
				{
					cp2 = new ChartPanel2(
							dssGrabber.getPlotTitle() + " - Difference from " + results[0][0].fileName,
							dssGrabber.getYLabel(), diffResults, false, mts);
					tabbedPane.insertTab("Difference", null, cp2, null, 0);
				}
				else if(doComparison)
				{
					cp1 = new ChartPanel2(dssGrabber.getPlotTitle() + " - Comparison ", dssGrabber.getYLabel(),
							results, false, mts);
					tabbedPane.insertTab("Comparison", null, cp1, null, 0);
				}
			}
		}
	}

	private void plotDts(DerivedTimeSeries dts, DSSGrabber2SvcImpl dssGrabber, JTabbedPane tabbedPane)
	{
		// Handle DTS

		dssGrabber.setLocation("@@" + dts.getName());

		TimeSeriesContainer[] primaryResults = dssGrabber.getPrimarySeries();
		TimeSeriesContainer[] secondaryResults = dssGrabber.getSecondarySeries();

		if(primaryResults != null)
		{
			dssGrabber.calcTAFforCFS(primaryResults, secondaryResults);

			TimeSeriesContainer[] diffResults = dssGrabber.getDifferenceSeries(primaryResults);
			DisplayInput displayInput = new DisplayInput(primaryResults, secondaryResults, diffResults);
			List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
			scenarioRuns.add(getBaseRun());
			scenarioRuns.addAll(getAlternatives());
			Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
			for(int i = 0; i < primaryResults.length; i++)
			{
				EpptScenarioRun epptScenarioRun = scenarioRuns.get(i);
				TimeSeriesContainer[] primarySeries = new TimeSeriesContainer[]{primaryResults[i]};
				scenarioRunData.put(epptScenarioRun, primarySeries);
			}

			String sLabel = dssGrabber.getSLabel();
			String baseRunName = dssGrabber.getBaseRunName();
			if(getPlotConfigurationState().isDisplayTimeSeriesPlot())
			{
				plotTimeSeries(scenarioRunData, null, tabbedPane);
			}
			if(getPlotConfigurationState().isDoExceedance())
			{
				plotExceedance(scenarioRunData, tabbedPane, dts.getName());
			}
			if(getPlotConfigurationState().isDisplayBoxAndWhiskerPlot())
			{
				plotBoxPlot(scenarioRunData, tabbedPane, null);
			}
			if(getPlotConfigurationState().isDisplayMonthlyTable())
			{
				plotMonthlyTable(dssGrabber, displayInput, tabbedPane, dts.getName(), sLabel, baseRunName);
			}
			if(getPlotConfigurationState().isDisplaySummaryTable())
			{
				plotSummaryTable(dssGrabber, displayInput, tabbedPane, dts.getName(), sLabel, baseRunName);
			}
		}
	}
}

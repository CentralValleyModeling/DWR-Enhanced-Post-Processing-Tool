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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
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
public class DisplayWrimsFrames extends DisplayFrames
{

	private static final Logger LOG = Logger.getLogger(DisplayGuiLinkFrames.class.getName());
	private static final IErrorHandlingSvc ERROR_HANDLING_SVC = new ErrorHandlingSvcImpl();
	private final DerivedTimeSeries _dts;
	private final MultipleTimeSeries _mts;

	DisplayWrimsFrames(PlotConfigurationState plotConfigurationState,
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
		boolean isCFS = !getPlotConfigurationState().isDisplayTaf();
		boolean doDifference = getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF;
		boolean doTimeSeries = getPlotConfigurationState().isDisplayTimeSeriesPlot();
		boolean doBase = getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.BASE;
		boolean doExceedance = getPlotConfigurationState().isDoExceedance();
		boolean doBoxPlot = getPlotConfigurationState().isDisplayBoxAndWhiskerPlot();
		boolean doMonthlyTable = getPlotConfigurationState().isDisplayMonthlyTable();
		boolean doSummaryTable = getPlotConfigurationState().isDisplaySummaryTable();
		List<String> summaryTags = getPlotConfigurationState().getSelectedSummaryTableItems();

		JTabbedPane tabbedPane = new JTabbedPane();

		dssGrabber.setIsCFS(isCFS);
		dssGrabber.setScenarioRuns(getBaseRun(), getAlternatives());

		dssGrabber.setDateRange(getStart(), getEnd());

		if(_mts != null)
		{
			plotMts(getAlternatives(), _mts, dssGrabber, tabbedPane);

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

	private void plotMts(List<EpptScenarioRun> scenarios, MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, JTabbedPane tabbedPane)
	{
		// Handle MTS

		dssGrabber.setLocation("@@" + mts.getName());

		int n = mts.getNumberOfDataReferences();
		int s = scenarios.size();
		Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarios::indexOf));
		TimeSeriesContainer[][] results = new TimeSeriesContainer[n][s];
		for(int i = 0; i < mts.getNumberOfDataReferences(); i++)
		{
			TimeSeriesContainer[] result = dssGrabber.getMultipleTimeSeries(i);
			for(int j = 0; j < scenarios.size(); j++)
			{
				EpptScenarioRun epptScenarioRun = scenarios.get(j);
				TimeSeriesContainer tsc = result[j];
				scenarioRunData.computeIfAbsent(epptScenarioRun, v -> new ArrayList<>()).add(tsc);
			}
		}

		dssGrabber.calcTAFforCFS(results);

		TimeSeriesContainer[][] diffResults = dssGrabber.getDifferenceSeriesWithMultipleTimeSeries(results);

		if(getPlotConfigurationState().isDisplayTimeSeriesPlot())
		{
			plotTimeSeries(scenarioRunData, new HashMap<>(), mts.getName(), tabbedPane);
		}
		if(getPlotConfigurationState().isDoExceedance())
		{
			plotExceedance(scenarioRunData, new HashMap<>(), mts.getName(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayBoxAndWhiskerPlot())
		{
			plotBoxPlot(scenarioRunData, new HashMap<>(), mts.getName(), tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayMonthlyTable())
		{
			mtsMonthly(mts, dssGrabber, tabbedPane, results, diffResults);
		}
		if(getPlotConfigurationState().isDisplaySummaryTable())
		{
			mtsSummary(mts, dssGrabber, tabbedPane, results, diffResults);
		}


	}

	private void mtsMonthly(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, JTabbedPane tabbedPane,
							TimeSeriesContainer[][] results, TimeSeriesContainer[][] diffResults)
	{
		MonthlyTablePanel2 mtp;
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			mtp = new MonthlyTablePanel2(diffResults, dssGrabber, mts);
		}
		else
		{
			mtp = new MonthlyTablePanel2(results, dssGrabber, mts);
		}
		tabbedPane.addTab("Monthly - " + dssGrabber.getBaseRunName(), mtp);
	}

	private void mtsSummary(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber,
							JTabbedPane tabbedPane, TimeSeriesContainer[][] results, TimeSeriesContainer[][] diffResults)
	{
		boolean doBase = getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.BASE;
		SummaryTablePanel2 stp;
		List<String> summaryTags = getPlotConfigurationState().getSelectedSummaryTableItems();
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
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
		tabbedPane.addTab("Summary - " + dssGrabber.getBaseRunName(), stp);
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
			Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
			for(int i = 0; i < primaryResults.length; i++)
			{
				EpptScenarioRun epptScenarioRun = scenarioRuns.get(i);
				TimeSeriesContainer tsc = primaryResults[i];
				scenarioRunData.put(epptScenarioRun, Collections.singletonList(tsc));
			}

			String sLabel = dssGrabber.getSLabel();
			String baseRunName = dssGrabber.getBaseRunName();
			if(getPlotConfigurationState().isDisplayTimeSeriesPlot())
			{
				plotTimeSeries(scenarioRunData, new HashMap<>(), dts.getName(), tabbedPane);
			}
			if(getPlotConfigurationState().isDoExceedance())
			{
				plotExceedance(scenarioRunData, new HashMap<>(), dts.getName(), tabbedPane);
			}
			if(getPlotConfigurationState().isDisplayBoxAndWhiskerPlot())
			{
				plotBoxPlot(scenarioRunData, new HashMap<>(), dts.getName(), tabbedPane);
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

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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import javax.swing.*;

import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-20-2020
 */
class DisplayWRIMSFrames extends DisplayFrames
{
	private static final IErrorHandlingSvc ERROR_HANDLING_SVC = new ErrorHandlingSvcImpl();
	private final DerivedTimeSeries _dts;
	private final MultipleTimeSeries _mts;

	DisplayWRIMSFrames(EpptConfigurationController epptConfigurationController,
					   PlotConfigurationState plotConfigurationState, DerivedTimeSeries dts,
					   MultipleTimeSeries mts)
	{
		super(epptConfigurationController, plotConfigurationState);
		_dts = dts;
		_mts = mts;
	}

	List<JTabbedPane> showDisplayFrames()
	{
		List<JTabbedPane> tabbedPanes = new ArrayList<>();
		Optional<EpptScenarioRun> baseRun = getBaseRun();
		if(baseRun.isPresent())
		{
			DSSGrabber2SvcImpl dssGrabber = new DSSGrabber2SvcImpl(_dts, _mts);
			JTabbedPane tabbedPane = new JTabbedPane();
			dssGrabber.setIsCFS(!getEpptConfigurationController().isTaf());
			dssGrabber.setScenarioRuns(baseRun.get(), getAlternatives());
			dssGrabber.setDateRange(getStart(), getEnd());
			List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
			scenarioRuns.add(baseRun.get());
			scenarioRuns.addAll(getAlternatives());
			if(_mts != null)
			{
				tabbedPane.setName(_mts.getName());
				plotMts(scenarioRuns, _mts, dssGrabber, tabbedPane);
			}
			if(_dts != null)
			{
				tabbedPane.setName(_dts.getName());
				plotDts(scenarioRuns, _dts, dssGrabber, tabbedPane);
			}
			Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
			if(!missing.isEmpty())
			{
				insertEmptyTab(tabbedPane, missing);
			}
			tabbedPanes.add(tabbedPane);
		}
		else
		{
			ERROR_HANDLING_SVC.businessErrorHandler("Must select Base Scenario", "");
		}
		return tabbedPanes;
	}

	private void plotMts(List<EpptScenarioRun> scenarios, MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber,
						 JTabbedPane tabbedPane)
	{
		dssGrabber.setLocation("@@" + mts.getName());
		int n = mts.getNumberOfDataReferences();
		Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarios::indexOf));
		for(int i = 0; i < n; i++)
		{
			TimeSeriesContainer[] result = dssGrabber.getMultipleTimeSeries(i);
			for(int j = 0; j < scenarios.size(); j++)
			{
				EpptScenarioRun epptScenarioRun = scenarios.get(j);
				TimeSeriesContainer tsc = result[j];
				if(tsc != null)
				{
					scenarioRunData.computeIfAbsent(epptScenarioRun, v -> new ArrayList<>()).add(tsc);
				}
			}
		}
		plot(tabbedPane, scenarioRunData, mts.getName());
	}

	private void plot(JTabbedPane tabbedPane, Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData, String name)
	{
		if(getPlotConfigurationState().isDisplayTimeSeriesAll())
		{
			plotTimeSeriesDiscrete(scenarioRunData, name, tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayTimeSeriesAggregate())
		{
			plotTimeSeriesAggregate(scenarioRunData, name, tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayExceedanceAll())
		{
			plotAllExceedance(scenarioRunData, name, tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayExceedanceAggregate())
		{
			plotAggregateExceedance(scenarioRunData, name, tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayBoxAndWhiskerAll())
		{
			plotBoxPlotDiscrete(scenarioRunData, name, tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayBoxAndWhiskerAggregate())
		{
			plotBoxPlotAggregate(scenarioRunData, name, tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayMonthlyTable())
		{
			plotMonthlyTable(scenarioRunData, name, tabbedPane);
		}
		if(getPlotConfigurationState().isDisplaySummaryTable())
		{
			plotSummaryTable(scenarioRunData, name, tabbedPane);
		}
	}

	private void plotDts(List<EpptScenarioRun> scenarioRuns, DerivedTimeSeries dts, DSSGrabber2SvcImpl dssGrabber, JTabbedPane tabbedPane)
	{
		dssGrabber.setLocation("@@" + dts.getName());

		TimeSeriesContainer[] primaryResults = dssGrabber.getPrimarySeries();
		if(primaryResults != null)
		{
			Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarioRuns::indexOf));
			for(int i = 0; i < primaryResults.length; i++)
			{
				EpptScenarioRun epptScenarioRun = scenarioRuns.get(i);
				TimeSeriesContainer tsc = primaryResults[i];
				if(tsc != null)
				{
					scenarioRunData.put(epptScenarioRun, Collections.singletonList(tsc));
				}
			}
			plot(tabbedPane, scenarioRunData, dts.getName());
		}
	}
}

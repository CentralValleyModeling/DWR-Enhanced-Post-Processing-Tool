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
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
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
			Optional<String> error = getError();
			if(error.isPresent())
			{
				DialogSvcImpl.getDialogSvcInstance().getOK("Unable to generate plots - " + error.get(), JOptionPane.WARNING_MESSAGE);
			}
			else
			{
				JTabbedPane tabbedPane = new JTabbedPane();
				List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
				scenarioRuns.add(baseRun.get());
				scenarioRuns.addAll(getAlternatives());
				if(_mts != null)
				{
					tabbedPane.setName(_mts.getName());
					plotMts(scenarioRuns, _mts, tabbedPane);
				}
				if(_dts != null)
				{
					tabbedPane.setName(_dts.getName());
					plotDts(scenarioRuns, _dts, tabbedPane);
				}
				DSSGrabber2SvcImpl dssGrabber = buildDssGrabber(baseRun.get());
				Map<GUILinksAllModelsBO.Model, List<String>> missing = dssGrabber.getMissingList();
				if(!missing.isEmpty())
				{
					insertEmptyTab(tabbedPane, missing);
				}
				tabbedPanes.add(tabbedPane);
			}
		}
		else
		{
			ERROR_HANDLING_SVC.businessErrorHandler("Must select Base Scenario", "");
		}
		return tabbedPanes;
	}

	private DSSGrabber2SvcImpl buildDssGrabber(EpptScenarioRun run)
	{
		DSSGrabber2SvcImpl dssGrabber = new DSSGrabber2SvcImpl(_dts, _mts);
		dssGrabber.setIsCFS(!getEpptConfigurationController().isTaf());
		dssGrabber.setScenarioRuns(run, Collections.emptyList());
		dssGrabber.setDateRange(getStart(), getEnd());
		return dssGrabber;
	}

	private void plotMts(List<EpptScenarioRun> scenarios, MultipleTimeSeries mts, JTabbedPane tabbedPane)
	{

		Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData;
		if(getEpptConfigurationController().isDifference())
		{
			scenarioRunData = getMtsDiffData(scenarios, mts);
		}
		else
		{
			scenarioRunData = getMtsComparisonData(scenarios, mts);
		}
		plot(tabbedPane, scenarioRunData, mts.getName());
	}

	private Map<EpptScenarioRun, List<TimeSeriesContainer>> getMtsDiffData(List<EpptScenarioRun> scenarios, MultipleTimeSeries mts)
	{
		Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarios::indexOf));
		Optional<EpptScenarioRun> base = getEpptConfigurationController().getEpptScenarioBase();
		if(base.isPresent())
		{
			TimeSeriesContainer[] baseContainers = getBaseMtsData(mts, base.get());
			for(EpptScenarioRun epptScenarioRun : scenarios)
			{
				if(!epptScenarioRun.isBaseSelected())
				{
					for(int i = 0; i < mts.getNumberOfDataReferences(); i++)
					{
						DSSGrabber2SvcImpl dssGrabber = buildDssGrabber(epptScenarioRun);
						dssGrabber.setLocation("@@" + mts.getName());
						TimeSeriesContainer[] result = dssGrabber.getMultipleTimeSeries(i);
						TimeSeriesContainer tsc = DSSGrabber1SvcImpl.diffSeries(baseContainers[i], result[0]);
						scenarioRunData.computeIfAbsent(epptScenarioRun, v -> new ArrayList<>()).add(tsc);
					}
				}
			}
		}
		return scenarioRunData;
	}

	private TimeSeriesContainer[] getBaseMtsData(MultipleTimeSeries mts, EpptScenarioRun base)
	{
		TimeSeriesContainer[] baseContainers = new TimeSeriesContainer[mts.getNumberOfDataReferences()];
		for(int i = 0; i < mts.getNumberOfDataReferences(); i++)
		{
			DSSGrabber2SvcImpl dssGrabber = buildDssGrabber(base);
			dssGrabber.setLocation("@@" + mts.getName());
			TimeSeriesContainer[] result = dssGrabber.getMultipleTimeSeries(i);
			TimeSeriesContainer tsc = result[0];
			baseContainers[i] = tsc;
		}
		return baseContainers;
	}

	private Map<EpptScenarioRun, List<TimeSeriesContainer>> getMtsComparisonData(List<EpptScenarioRun> scenarios, MultipleTimeSeries mts)
	{
		Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData = new TreeMap<>(Comparator.comparing(scenarios::indexOf));
		for(EpptScenarioRun epptScenarioRun : scenarios)
		{
			for(int i = 0; i < mts.getNumberOfDataReferences(); i++)
			{
				DSSGrabber2SvcImpl dssGrabber = buildDssGrabber(epptScenarioRun);
				dssGrabber.setLocation("@@" + mts.getName());
				TimeSeriesContainer[] result = dssGrabber.getMultipleTimeSeries(i);
				TimeSeriesContainer tsc = result[0];
				if(tsc != null)
				{
					scenarioRunData.computeIfAbsent(epptScenarioRun, v -> new ArrayList<>()).add(tsc);
				}
			}
		}
		return scenarioRunData;
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
		if(getPlotConfigurationState().isDisplayMonthlyLine())
		{
			plotMonthlyLine(scenarioRunData, name, tabbedPane);
		}
		if(getPlotConfigurationState().isDisplayBarCharts())
		{
			plotBarCharts(scenarioRunData, name, tabbedPane);
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

	private void plotDts(List<EpptScenarioRun> scenarios, DerivedTimeSeries dts, JTabbedPane tabbedPane)
	{
		Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData;
		if(getEpptConfigurationController().isDifference())
		{
			scenarioRunData = getDiffDtsData(scenarios, dts);
		}
		else
		{
			scenarioRunData = getComparisonDtsData(scenarios, dts);
		}
		plot(tabbedPane, scenarioRunData, dts.getName());
	}

	private Map<EpptScenarioRun, List<TimeSeriesContainer>> getDiffDtsData(List<EpptScenarioRun> scenarios, DerivedTimeSeries dts)
	{
		final Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData;
		scenarioRunData  = new TreeMap<>(Comparator.comparing(scenarios::indexOf));
		TimeSeriesContainer[] baseData = getBaseDtsData(scenarios, dts);
		if(baseData != null)
		{
			for(EpptScenarioRun epptScenarioRun : scenarios)
			{
				if(!epptScenarioRun.isBaseSelected())
				{
					DSSGrabber2SvcImpl dssGrabber = buildDssGrabber(epptScenarioRun);
					dssGrabber.setLocation("@@" + dts.getName());
					TimeSeriesContainer[] result = dssGrabber.getPrimarySeries();
					if(result[0] != null)
					{
						TimeSeriesContainer tsc = result[0];
						tsc.setFullName(epptScenarioRun.getName() + " (" + epptScenarioRun.getModel() + ")");
						tsc = DSSGrabber1SvcImpl.diffSeries(baseData[0], result[0]);
						scenarioRunData.computeIfAbsent(epptScenarioRun, v -> new ArrayList<>()).add(tsc);
					}
				}
			}
		}
		return scenarioRunData;
	}

	private TimeSeriesContainer[] getBaseDtsData(List<EpptScenarioRun> scenarios, DerivedTimeSeries dts)
	{
		TimeSeriesContainer[] baseData = null;
		for(EpptScenarioRun epptScenarioRun : scenarios)
		{
			if(epptScenarioRun.isBaseSelected())
			{
				DSSGrabber2SvcImpl dssGrabber = buildDssGrabber(epptScenarioRun);
				dssGrabber.setLocation("@@" + dts.getName());
				TimeSeriesContainer[] result = dssGrabber.getPrimarySeries();
				if(result[0] != null)
				{
					baseData = result;
				}
			}
		}
		return baseData;
	}

	private Map<EpptScenarioRun, List<TimeSeriesContainer>> getComparisonDtsData(List<EpptScenarioRun> scenarios, DerivedTimeSeries dts)
	{
		final Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData;
		scenarioRunData = new TreeMap<>(Comparator.comparing(scenarios::indexOf));
		for(EpptScenarioRun epptScenarioRun : scenarios)
		{
			DSSGrabber2SvcImpl dssGrabber = buildDssGrabber(epptScenarioRun);
			dssGrabber.setLocation("@@" + dts.getName());
			TimeSeriesContainer[] result = dssGrabber.getPrimarySeries();
			if(result[0] != null)
			{
				TimeSeriesContainer tsc = result[0];
				tsc.setFullName(epptScenarioRun.getName() + " (" + epptScenarioRun.getModel() + ")");
				scenarioRunData.computeIfAbsent(epptScenarioRun, v -> new ArrayList<>()).add(tsc);
			}
		}
		return scenarioRunData;
	}
}

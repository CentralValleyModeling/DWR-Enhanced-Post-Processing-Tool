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

package gov.ca.water.calgui.presentation.plotly;

import java.time.LocalDate;
import java.time.Month;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.busservice.impl.NoopEpptStatistic;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-14-2020
 */
public class PlotlyPaneBuilder
{
	private final ChartType _chartType;
	private final Map<EpptScenarioRun, TimeSeriesContainer[]> _scenarioRunData;
	private final EpptScenarioRun _baseRun;
	private final Map<EpptScenarioRun, List<WaterYearIndex>> _allWaterYearIndicies = new HashMap<>();
	private final Map<EpptScenarioRun, WaterYearIndex> _selectedWaterYearIndicies = new HashMap<>();
	private PlotConfigurationState.ComparisonType _comparisonType = PlotConfigurationState.ComparisonType.COMPARISON;
	private boolean _taf;
	private LocalDate _start;
	private LocalDate _end;
	private WaterYearDefinition _waterYearDefinition;
	private Month _month;
	private String _plotTitle;

	public PlotlyPaneBuilder(ChartType chartType, EpptScenarioRun baseRun, Map<EpptScenarioRun, TimeSeriesContainer[]> scenarioRunData)
	{
		_chartType = chartType;
		_scenarioRunData = scenarioRunData;
		if(scenarioRunData.isEmpty())
		{
			throw new IllegalArgumentException("Cannot plot without scenarios");
		}
		_baseRun = baseRun;
	}

	public PlotlyPaneBuilder withComparisonType(PlotConfigurationState.ComparisonType comparisonType)
	{
		_comparisonType = comparisonType;
		return this;
	}

	public PlotlyPaneBuilder withPlotTitle(String plotTitle)
	{
		_plotTitle = plotTitle;
		return this;
	}

	public PlotlyPaneBuilder withTaf(boolean taf)
	{
		_taf = taf;
		return this;
	}

	public PlotlyPaneBuilder withTimeWindow(LocalDate start, LocalDate end)
	{
		_start = start;
		_end = end;
		return this;
	}

	public PlotlyPaneBuilder withWaterYearIndicies(Map<EpptScenarioRun, List<WaterYearIndex>> waterYearIndicies)
	{
		_allWaterYearIndicies.clear();
		_allWaterYearIndicies.putAll(waterYearIndicies);
		_selectedWaterYearIndicies.clear();
		for(Map.Entry<EpptScenarioRun, List<WaterYearIndex>> entry : waterYearIndicies.entrySet())
		{
			if(!entry.getValue().isEmpty())
			{
				_selectedWaterYearIndicies.put(entry.getKey(), entry.getValue().get(0));
			}
		}
		return this;
	}

	public PlotlyPane build()
	{
		EpptReportingComputedSet epptReportingComputedSet;
		if(_comparisonType == PlotConfigurationState.ComparisonType.DIFF)
		{
			epptReportingComputedSet = EpptReportingComputedSet.computeDiffForMetrics(_plotTitle,
					new NoopEpptStatistic(),
					new MonthPeriod(_waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth()),
					_start,
					_end,
					_taf,
					_baseRun,
					_scenarioRunData,
					_comparisonType,
					_selectedWaterYearIndicies,
					_allWaterYearIndicies);
		}
		else
		{
			epptReportingComputedSet = EpptReportingComputedSet.computeForMetrics(_plotTitle,
					new NoopEpptStatistic(),
					new MonthPeriod(_waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth()),
					_start,
					_end,
					_taf,
					_scenarioRunData,
					_comparisonType,
					_selectedWaterYearIndicies,
					_allWaterYearIndicies);
		}
		PlotlyPane retval = null;
		switch(_chartType)
		{
			case TIMESERIES:
				retval = new TimeseriesChartPane(epptReportingComputedSet);
				break;
			case EXCEEDANCE:
				retval = buildExceedancePane(epptReportingComputedSet);
				break;
			case BOX:
				retval = new BoxPlotChartPane(epptReportingComputedSet);
				break;
		}
		return retval;
	}

	private PlotlyPane buildExceedancePane(EpptReportingComputedSet epptReportingComputedSet)
	{
		final PlotlyPane retval;
		if(_waterYearDefinition == null)
		{
			retval = new AllExceedancePane(epptReportingComputedSet);
		}
		else if(_month != null)
		{
			retval = new MonthExceedancePane(epptReportingComputedSet);
		}
		else
		{
			retval = new AnnualExceedancePane(epptReportingComputedSet);
		}
		return retval;
	}

	public PlotlyPaneBuilder withWaterYearDefinition(WaterYearDefinition waterYearDefinition)
	{
		_waterYearDefinition = waterYearDefinition;
		return this;
	}

	public PlotlyPaneBuilder withMonth(Month month)
	{
		_waterYearDefinition = new WaterYearDefinition("Month", month, month);
		_month = month;
		return this;
	}

	public enum ChartType
	{
		TIMESERIES, EXCEEDANCE, BOX
	}
}

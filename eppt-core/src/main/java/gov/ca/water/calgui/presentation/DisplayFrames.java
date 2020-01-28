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
import java.util.Optional;
import java.util.SortedMap;
import java.util.TreeMap;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.busservice.impl.NoopEpptStatistic;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.presentation.plotly.PlotlyPaneBuilder;
import gov.ca.water.calgui.presentation.plotly.SummaryPane;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import javafx.embed.swing.JFXPanel;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-20-2020
 */
public class DisplayFrames
{
	private final PlotConfigurationState _plotConfigurationState;
	private final EpptScenarioRun _baseRun;
	private final List<EpptScenarioRun> _alternatives;
	private final LocalDate _start;
	private final LocalDate _end;
	private final Map<EpptScenarioRun, List<WaterYearIndex>> _waterYearIndicies;

	DisplayFrames(PlotConfigurationState plotConfigurationState,
				  EpptScenarioRun baseRun,
				  List<EpptScenarioRun> alternatives,
				  LocalDate start,
				  LocalDate end) throws EpptInitializationException
	{
		_plotConfigurationState = plotConfigurationState;
		_baseRun = baseRun;
		_alternatives = alternatives;
		_start = start;
		_end = end;

		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(_baseRun);
		scenarioRuns.addAll(_alternatives);
		_waterYearIndicies = new HashMap<>();
		for(EpptScenarioRun epptScenarioRun : scenarioRuns)
		{
			WaterYearTableReader tableReader = new WaterYearTableReader(epptScenarioRun.getLookupDirectory());
			List<WaterYearIndex> read = tableReader.read();
			_waterYearIndicies.put(epptScenarioRun, read);
		}
	}

	PlotConfigurationState getPlotConfigurationState()
	{
		return _plotConfigurationState;
	}

	LocalDate getEnd()
	{
		return _end;
	}

	LocalDate getStart()
	{
		return _start;
	}

	private Map<EpptScenarioRun, List<WaterYearIndex>> getWaterYearIndicies()
	{
		return _waterYearIndicies;
	}

	EpptScenarioRun getBaseRun()
	{
		return _baseRun;
	}

	List<EpptScenarioRun> getAlternatives()
	{
		return _alternatives;
	}

	void plotBoxPlot(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
					 Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
					 String plotTitle,
					 JTabbedPane tabbedPane)
	{
		plotBoxPlot(new HashMap<>(), scenarioRunData, new HashMap<>(), secondaryScenarioRunData, plotTitle, tabbedPane);
	}

	void plotBoxPlot(Map<EpptScenarioRun, List<String>> primarySuffixes,
					 Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
					 Map<EpptScenarioRun, List<String>> secondarySuffixes,
					 Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
					 String plotTitle,
					 JTabbedPane tabbedPane)
	{

		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.BOX, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withWaterYearDefinition(getPlotConfigurationState().getWaterYearDefinition())
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withPrimarySuffixes(primarySuffixes)
				.withSecondarySuffixes(secondarySuffixes)
				.withPlotTitle(plotTitle)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.build();
		tabbedPane.addTab("Box Plot", pane);
	}

	void plotTimeSeries(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
						String plotTitle, JTabbedPane tabbedPane)
	{
		plotTimeSeries(new HashMap<>(), scenarioRunData, new HashMap<>(), secondaryScenarioRunData, plotTitle, tabbedPane);
	}

	void plotTimeSeries(Map<EpptScenarioRun, List<String>> primarySuffixes,
						Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						Map<EpptScenarioRun, List<String>> secondarySuffixes,
						Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
						String plotTitle, JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.TIMESERIES, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withWaterYearDefinition(getPlotConfigurationState().getWaterYearDefinition())
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withPrimarySuffixes(primarySuffixes)
				.withSecondarySuffixes(secondarySuffixes)
				.withPlotTitle(plotTitle)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.build();
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Time Series", pane);
		}
	}

	void insertEmptyTab(JTabbedPane tabbedPane, Map<GUILinksAllModelsBO.Model, List<String>> missing)
	{
		StringBuilder buffer = new StringBuilder();
		buffer.append("<html><br>Not all DSS records were found, some results may be missing:<br><br>");
		missing.forEach((key, value) -> buffer.append("Model: ").append(key).append("<br>").append(
				String.join("<br>", value)).append("<br>"));
		buffer.append("</html>");
		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(new JLabel(buffer.toString()), BorderLayout.PAGE_START);
		tabbedPane.addTab("Alert - Missing DSS records", panel);
	}

	void plotSummaryTable(Map<EpptScenarioRun, List<String>> primarySuffixes,
						  Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						  Map<EpptScenarioRun, List<String>> secondarySuffixes,
						  Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
						  String plotTitle,
						  JTabbedPane tabbedPane)
	{
		SortedMap<String, SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>>> data = new TreeMap<>();
		List<EpptStatistic> selectedSummaryTableItems = getPlotConfigurationState().getSelectedSummaryTableItems();
		WaterYearDefinition waterYearDefinition = getPlotConfigurationState().getWaterYearDefinition();
		MonthPeriod monthPeriod = new MonthPeriod(waterYearDefinition.getStartMonth(), waterYearDefinition.getEndMonth());
		List<WaterYearIndex> summaryWaterYearIndexes = getPlotConfigurationState().getSummaryWaterYearIndexes();
		SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>> statsData = new TreeMap<>(
				Comparator.comparing(selectedSummaryTableItems::indexOf));
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(getBaseRun());
		scenarioRuns.addAll(getAlternatives());
		for(EpptScenarioRun epptScenarioRun : getAlternatives())
		{
			Map<EpptScenarioRun, List<String>> clonedSuffix = new HashMap<>();
			if(!primarySuffixes.isEmpty())
			{
				clonedSuffix.put(epptScenarioRun, primarySuffixes.get(epptScenarioRun));
			}
			Map<EpptScenarioRun, List<TimeSeriesContainer>> clonedScenarioRunData = new HashMap<>();
			if(!scenarioRunData.isEmpty())
			{
				clonedScenarioRunData.put(epptScenarioRun, scenarioRunData.get(epptScenarioRun));
			}
			Map<EpptScenarioRun, List<String>> clonedSecondarySuffix = new HashMap<>();
			if(!secondarySuffixes.isEmpty())
			{
				clonedSecondarySuffix.put(epptScenarioRun, secondarySuffixes.get(epptScenarioRun));
			}
			Map<EpptScenarioRun, List<TimeSeriesContainer>> clonedSecondaryScenarioRunData = new HashMap<>();
			if(!secondaryScenarioRunData.isEmpty())
			{
				clonedSecondaryScenarioRunData.put(epptScenarioRun, secondaryScenarioRunData.get(epptScenarioRun));
			}
			addSummaryDataForScenario(clonedSuffix, clonedScenarioRunData, plotTitle, data, selectedSummaryTableItems,
					monthPeriod, summaryWaterYearIndexes, statsData, epptScenarioRun);
			addSummaryDataForScenario(clonedSecondarySuffix, clonedSecondaryScenarioRunData, plotTitle, data, selectedSummaryTableItems,
					monthPeriod, summaryWaterYearIndexes, statsData, epptScenarioRun);
		}
		JFXPanel pane = new SummaryPane(plotTitle, waterYearDefinition, data);
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Summary - Difference", pane);
		}
		else

		{
			tabbedPane.addTab("Summary", pane);
		}

	}

	private void addSummaryDataForScenario(Map<EpptScenarioRun, List<String>> primarySuffixes,
										   Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData, String plotTitle,
										   SortedMap<String, SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>>> data,
										   List<EpptStatistic> selectedSummaryTableItems, MonthPeriod monthPeriod,
										   List<WaterYearIndex> summaryWaterYearIndexes,
										   SortedMap<EpptStatistic, Map<WaterYearIndex, EpptReportingComputedSet>> statsData, EpptScenarioRun scenarioRun)
	{
		for(int i = 0; i < scenarioRunData.get(scenarioRun).size(); i++)
		{
			String header = scenarioRun.getName();
			if(i < primarySuffixes.getOrDefault(scenarioRun, new ArrayList<>()).size())
			{
				header += " - " + primarySuffixes.get(scenarioRun).get(i);
			}
			else
			{
				header += " - " + plotTitle;
			}
			for(EpptStatistic stat : selectedSummaryTableItems)
			{
				Map<EpptScenarioRun, List<WaterYearIndex>> allWaterYearIndicies = getWaterYearIndicies();
				SortedMap<WaterYearIndex, EpptReportingComputedSet> indexData = new TreeMap<>(
						Comparator.comparing(allWaterYearIndicies.get(getBaseRun())::indexOf));
				Map<EpptScenarioRun, WaterYearIndex> selectedWaterYearIndicies = new HashMap<>();
				for(WaterYearIndex waterYearIndex : summaryWaterYearIndexes)
				{
					for(Map.Entry<EpptScenarioRun, List<WaterYearIndex>> entry : allWaterYearIndicies.entrySet())
					{
						Optional<WaterYearIndex> collect = entry.getValue().stream().filter(
								s -> waterYearIndex.getName().equalsIgnoreCase(s.getName())).findAny();
						collect.ifPresent(index -> selectedWaterYearIndicies.put(entry.getKey(), index));
					}
					EpptReportingComputedSet epptReportingComputedSet;
					if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
					{
						epptReportingComputedSet = EpptReportingComputedSet.computeDiffForMetrics(plotTitle,
								stat,
								monthPeriod,
								_start,
								_end,
								getPlotConfigurationState().isDisplayTaf(),
								_baseRun,
								primarySuffixes,
								scenarioRunData,
								new HashMap<>(),
								new HashMap<>(),
								getPlotConfigurationState().getComparisonType(),
								selectedWaterYearIndicies,
								allWaterYearIndicies);
					}
					else
					{
						epptReportingComputedSet = EpptReportingComputedSet.computeForMetrics(plotTitle,
								stat,
								monthPeriod,
								_start,
								_end,
								getPlotConfigurationState().isDisplayTaf(),
								primarySuffixes,
								scenarioRunData,
								new HashMap<>(),
								new HashMap<>(),
								getPlotConfigurationState().getComparisonType(),
								selectedWaterYearIndicies,
								allWaterYearIndicies);
					}
					indexData.put(waterYearIndex, epptReportingComputedSet);
				}
				statsData.put(stat, indexData);
			}
			data.put(header, statsData);
		}
	}

	void plotMonthlyTable(Map<EpptScenarioRun, List<String>> primarySuffixes,
						  Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						  Map<EpptScenarioRun, List<String>> secondarySuffixes,
						  Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
						  String plotTitle,
						  JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.MONTHLY, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withWaterYearDefinition(getPlotConfigurationState().getWaterYearDefinition())
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withPrimarySuffixes(primarySuffixes)
				.withSecondarySuffixes(secondarySuffixes)
				.withPlotTitle(plotTitle)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.build();
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Monthly - Difference", pane);
		}
		else
		{
			tabbedPane.addTab("Monthly", pane);
		}
	}

	void plotExceedance(Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
						String plotTitle, JTabbedPane tabbedPane)
	{
		plotExceedance(new HashMap<>(), scenarioRunData, new HashMap<>(), secondaryScenarioRunData, plotTitle, tabbedPane);
	}

	void plotExceedance(Map<EpptScenarioRun, List<String>> primarySuffixes,
						Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
						Map<EpptScenarioRun, List<String>> secondarySuffixes,
						Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
						String plotTitle, JTabbedPane tabbedPane)
	{
		if(getPlotConfigurationState().isPlotAllExceedancePlots())
		{
			plotAllExceedance(primarySuffixes, scenarioRunData, secondarySuffixes, secondaryScenarioRunData, plotTitle, tabbedPane);
		}
		if(getPlotConfigurationState().isAnnualFlowExceedancePlots())
		{
			plotAnnualExceedance(primarySuffixes, scenarioRunData, secondarySuffixes, secondaryScenarioRunData, plotTitle, tabbedPane);
		}
		if(!getPlotConfigurationState().getSelectedExceedancePlotMonths().isEmpty())
		{
			plotMonthlyExceedance(primarySuffixes, scenarioRunData, secondarySuffixes, secondaryScenarioRunData, plotTitle, tabbedPane);
		}
	}

	private void plotMonthlyExceedance(Map<EpptScenarioRun, List<String>> primarySuffixes,
									   Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
									   Map<EpptScenarioRun, List<String>> secondarySuffixes,
									   Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
									   String plotTitle, JTabbedPane tabbedPane)
	{
		List<Month> exceedMonths = getPlotConfigurationState().getSelectedExceedancePlotMonths();
		for(Month month : exceedMonths)
		{
			JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
					.withComparisonType(getPlotConfigurationState().getComparisonType())
					.withTaf(getPlotConfigurationState().isDisplayTaf())
					.withPlotTitle(plotTitle)
					.withPrimarySuffixes(primarySuffixes)
					.withSecondarySuffixes(secondarySuffixes)
					.withTimeWindow(getStart(), getEnd())
					.withWaterYearIndicies(getWaterYearIndicies())
					.withMonth(month)
					.build();
			tabbedPane.addTab("Exceedance (" + month.getDisplayName(TextStyle.SHORT, Locale.getDefault()) + ")", pane);
		}
	}

	private void plotAllExceedance(Map<EpptScenarioRun, List<String>> primarySuffixes,
								   Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
								   Map<EpptScenarioRun, List<String>> secondarySuffixes,
								   Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
								   String plotTitle, JTabbedPane tabbedPane)
	{
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withPlotTitle(plotTitle)
				.withPrimarySuffixes(primarySuffixes)
				.withSecondarySuffixes(secondarySuffixes)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.build();
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Exceedance (All - Difference)", pane);
		}
		else
		{
			tabbedPane.addTab("Exceedance (All)", pane);
		}
	}

	private void plotAnnualExceedance(Map<EpptScenarioRun, List<String>> primarySuffixes,
									  Map<EpptScenarioRun, List<TimeSeriesContainer>> scenarioRunData,
									  Map<EpptScenarioRun, List<String>> secondarySuffixes,
									  Map<EpptScenarioRun, List<TimeSeriesContainer>> secondaryScenarioRunData,
									  String plotTitle, JTabbedPane tabbedPane)
	{
		WaterYearDefinition waterYearDefinition = getPlotConfigurationState().getWaterYearDefinition();
		JFXPanel pane = new PlotlyPaneBuilder(PlotlyPaneBuilder.ChartType.EXCEEDANCE, getBaseRun(), scenarioRunData, secondaryScenarioRunData)
				.withComparisonType(getPlotConfigurationState().getComparisonType())
				.withPrimarySuffixes(primarySuffixes)
				.withSecondarySuffixes(secondarySuffixes)
				.withTaf(getPlotConfigurationState().isDisplayTaf())
				.withPlotTitle(plotTitle)
				.withTimeWindow(getStart(), getEnd())
				.withWaterYearIndicies(getWaterYearIndicies())
				.withWaterYearDefinition(waterYearDefinition)
				.build();
		if(getPlotConfigurationState().getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			tabbedPane.addTab("Exceedance (Annual Total - Difference)", pane);
		}
		else
		{
			tabbedPane.addTab("Exceedance (Annual Total)", pane);
		}
	}
}

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

package gov.ca.water.calgui.project;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import vista.report.MonthlyReport;

public class PlotConfigurationState
{

	private static final String[] MONTH_NAMES = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
			"Dec"};

	private final ComparisonType _comparisonType;
	private final boolean _isDisplayTaf;
	private final boolean _isDisplayTimeSeriesPlot;
	private final boolean _isDisplayBoxAndWhiskerPlot;
	private final boolean _isDisplayMonthlyTable;
	private final boolean _isDisplaySummaryTable;
	private final List<String> _exceedancePlotComponents;
	private final List<String> _summaryTableComponents;

	public PlotConfigurationState(ComparisonType comparisonType, boolean isDisplayTaf, boolean isDisplayTimeSeriesPlot, boolean isDisplayBoxAndWhiskerPlot,
						   List<String> exceedancePlotComponents, boolean isDisplayMonthlyTable,
						   boolean isDisplaySummaryTable, List<String> summaryTableComponents)
	{
		_comparisonType = comparisonType;
		_isDisplayTaf = isDisplayTaf;
		_isDisplayTimeSeriesPlot = isDisplayTimeSeriesPlot;
		_isDisplayBoxAndWhiskerPlot = isDisplayBoxAndWhiskerPlot;
		_exceedancePlotComponents = exceedancePlotComponents;

		_isDisplayMonthlyTable = isDisplayMonthlyTable;
		_isDisplaySummaryTable = isDisplaySummaryTable;

		_summaryTableComponents = summaryTableComponents;
	}

	public static PlotConfigurationState fromString(String displayGroup)
	{
		boolean doTimeSeries = false;
		boolean doBoxPlot = false;
		boolean isCFS = false;
		boolean doMonthlyTable = false;
		boolean doSummaryTable = false;
		List<String> exceedMonths = new ArrayList<>();
		List<String> summaryTags = new ArrayList<>();

		String[] groupParts = displayGroup.split(";");

		ComparisonType comparisonType = ComparisonType.BASE;
		for(final String groupPart : groupParts)
		{
			if("Base".equals(groupPart))
			{
				comparisonType = ComparisonType.BASE;
			}
			if("Comp".equals(groupPart))
			{
				comparisonType = ComparisonType.COMPARISON;
			}
			else if("Diff".equals(groupPart))
			{
				comparisonType = ComparisonType.DIFF;
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
				exceedMonths = Arrays.asList(groupPart.substring(3).split(","));
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
				summaryTags = Arrays.asList(groupPart.substring(4).split(","));
			}
		}
		return new PlotConfigurationState(comparisonType, !isCFS, doTimeSeries, doBoxPlot, exceedMonths, doMonthlyTable, doSummaryTable, summaryTags);
	}

	public ComparisonType getComparisonType()
	{
		return _comparisonType;
	}

	public boolean isDisplayTaf()
	{
		return _isDisplayTaf;

	}

	public boolean isDisplayTimeSeriesPlot()
	{
		return _isDisplayTimeSeriesPlot;
	}

	public boolean isDisplayBoxAndWhiskerPlot()
	{
		return _isDisplayBoxAndWhiskerPlot;
	}

	public boolean isDisplayMonthlyTable()
	{
		return _isDisplayMonthlyTable;
	}

	public boolean isDisplaySummaryTable()
	{
		return _isDisplaySummaryTable;
	}

	public boolean isPlotAllExceedancePlots()
	{
		for(String name : _exceedancePlotComponents)
		{
			if("ALL".equals(name))
			{
				return true;
			}
		}
		return false;
	}

	public boolean isAnnualFlowExceedancePlots()
	{
		for(String name : _exceedancePlotComponents)
		{
			if("Annual Flow".equals(name))
			{
				return true;
			}
		}
		return false;
	}

	public List<String> getSelectedExceedancePlotMonths()
	{
		List<String> selectedMonths = new ArrayList<>();
		for(String name : _exceedancePlotComponents)
		{
			if(!"Annual Flow".equals(name) && !"ALL".equals(name))
			{
				selectedMonths.add(name);
			}
		}
		if(_exceedancePlotComponents.contains("ALL"))
		{
			selectedMonths.clear();
			selectedMonths.addAll(Arrays.asList(MonthlyReport.months));
		}
		return selectedMonths;
	}

	public List<String> getSelectedSummaryTableItems()
	{
		return _summaryTableComponents;
	}

	public boolean isDoExceedance()
	{
		return !getSelectedExceedancePlotMonths().isEmpty() || isAnnualFlowExceedancePlots() || isPlotAllExceedancePlots();
	}

	public enum ComparisonType
	{
		BASE, COMPARISON, DIFF
	}


}

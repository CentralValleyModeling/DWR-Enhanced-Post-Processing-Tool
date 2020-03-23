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

public class PlotConfigurationState
{

	private final boolean _displayTimeSeriesAll;
	private final boolean _displayTimeSeriesAggregate;
	private final boolean _displayExceedanceAll;
	private final boolean _displayExceedanceAggregate;
	private final boolean _displayBoxAndWhiskerAll;
	private final boolean _displayBoxAndWhiskerAggregate;
	private final boolean _displayMonthlyTable;
	private final boolean _displaySummaryTable;
	private final boolean _displayMonthlyLine;
	private final boolean _displayBarCharts;

	public PlotConfigurationState(boolean displayTimeSeriesAll, boolean displayTimeSeriesAggregate, boolean displayExceedanceAll,
								  boolean displayExceedanceAggregate, boolean displayBoxAndWhiskerAll, boolean displayBoxAndWhiskerAggregate,
								  boolean displayMonthlyTable, boolean displaySummaryTable, boolean displayMonthlyLine, boolean displayBarCharts)
	{
		_displayTimeSeriesAll = displayTimeSeriesAll;
		_displayTimeSeriesAggregate = displayTimeSeriesAggregate;
		_displayExceedanceAll = displayExceedanceAll;
		_displayExceedanceAggregate = displayExceedanceAggregate;
		_displayBoxAndWhiskerAll = displayBoxAndWhiskerAll;
		_displayBoxAndWhiskerAggregate = displayBoxAndWhiskerAggregate;
		_displayMonthlyTable = displayMonthlyTable;
		_displaySummaryTable = displaySummaryTable;
		_displayMonthlyLine = displayMonthlyLine;
		_displayBarCharts = displayBarCharts;
	}

	public boolean isDisplayTimeSeriesAll()
	{
		return _displayTimeSeriesAll;
	}

	public boolean isDisplayTimeSeriesAggregate()
	{
		return _displayTimeSeriesAggregate;
	}

	public boolean isDisplayExceedanceAll()
	{
		return _displayExceedanceAll;
	}

	public boolean isDisplayExceedanceAggregate()
	{
		return _displayExceedanceAggregate;
	}

	public boolean isDisplayBoxAndWhiskerAll()
	{
		return _displayBoxAndWhiskerAll;
	}

	public boolean isDisplayBoxAndWhiskerAggregate()
	{
		return _displayBoxAndWhiskerAggregate;
	}

	public boolean isDisplayMonthlyTable()
	{
		return _displayMonthlyTable;
	}

	public boolean isDisplaySummaryTable()
	{
		return _displaySummaryTable;
	}

	public boolean isDisplayBarCharts()
	{
		return _displayBarCharts;
	}

	public boolean isDisplayMonthlyLine()
	{
		return _displayMonthlyLine;
	}
}

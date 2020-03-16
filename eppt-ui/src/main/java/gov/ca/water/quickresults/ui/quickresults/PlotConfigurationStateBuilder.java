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

package gov.ca.water.quickresults.ui.quickresults;

import javax.swing.*;

import gov.ca.water.calgui.presentation.plotly.EpptPlotException;
import gov.ca.water.calgui.project.PlotConfigurationState;
import org.swixml.SwingEngine;

public class PlotConfigurationStateBuilder
{
	private static final String TIME_SERIES_ALL = "TimeSeriesAll";
	private static final String TIME_SERIES_AGG = "TimeSeriesAggregate";
	private static final String EXCEEDANCE_ALL = "ExceedanceAll";
	private static final String EXCEEDANCE_AGG = "ExceedanceAggregate";
	private static final String BOX_ALL = "BoxAndWhiskerAll";
	private static final String BOX_AGG = "BoxAndWhiskerAggregate";
	private static final String MONTHLY_TABLE = "MonthlyTable";
	private static final String SUMMARY_TABLE = "SummaryTable";
	private static final String MONTHLY_LINE = "MonthlyLine";
	private static final String BAR_GRAPH = "BarGraph";

	private final SwingEngine _swingEngine;

	public PlotConfigurationStateBuilder(SwingEngine swingEngine)
	{
		_swingEngine = swingEngine;
	}

	String createQuickStateString()
	{
		String retval = "";
		boolean displayTimeSeriesAll = ((JCheckBox) _swingEngine.find(TIME_SERIES_ALL)).isSelected();
		if(displayTimeSeriesAll)
		{
			retval += TIME_SERIES_ALL + ",";
		}
		boolean displayTimeSeriesAggregate = ((JCheckBox) _swingEngine.find(TIME_SERIES_AGG)).isSelected();
		if(displayTimeSeriesAggregate)
		{
			retval += TIME_SERIES_AGG + ",";
		}
		boolean displayExceedanceAll = ((JCheckBox) _swingEngine.find(EXCEEDANCE_ALL)).isSelected();
		if(displayExceedanceAll)
		{
			retval += EXCEEDANCE_ALL + ",";
		}
		boolean displayExceedanceAggregate = ((JCheckBox) _swingEngine.find(EXCEEDANCE_AGG)).isSelected();
		if(displayExceedanceAggregate)
		{
			retval += EXCEEDANCE_AGG + ",";
		}
		boolean displayBoxAndWhiskerAll = ((JCheckBox) _swingEngine.find(BOX_ALL)).isSelected();
		if(displayBoxAndWhiskerAll)
		{
			retval += BOX_ALL + ",";
		}
		boolean displayBoxAndWhiskerAggregate = ((JCheckBox) _swingEngine.find(BOX_AGG)).isSelected();
		if(displayBoxAndWhiskerAggregate)
		{
			retval += BOX_AGG + ",";
		}
		boolean displayMonthlyTable = ((JCheckBox) _swingEngine.find(MONTHLY_TABLE)).isSelected();
		if(displayMonthlyTable)
		{
			retval += MONTHLY_TABLE + ",";
		}
		boolean displaySummaryTable = ((JCheckBox) _swingEngine.find(SUMMARY_TABLE)).isSelected();
		if(displaySummaryTable)
		{
			retval += SUMMARY_TABLE + ",";
		}
		boolean displayMonthlyLine = ((JCheckBox) _swingEngine.find(MONTHLY_LINE)).isSelected();
		if(displayMonthlyLine)
		{
			retval += MONTHLY_LINE + ",";
		}
		boolean displayBarCharts = ((JCheckBox) _swingEngine.find(BAR_GRAPH)).isSelected();
		if(displayBarCharts)
		{
			retval += BAR_GRAPH + ",";
		}
		if(!retval.isEmpty())
		{
			retval = retval.substring(0, retval.length() - 1);
		}
		return "Display-" + retval;
	}

	public PlotConfigurationState createPlotConfigurationState() throws EpptPlotException
	{
		try
		{
			// Base, Comparison and Difference
			boolean displayTimeSeriesAll = ((JCheckBox) _swingEngine.find(TIME_SERIES_ALL)).isSelected();
			boolean displayTimeSeriesAggregate = ((JCheckBox) _swingEngine.find(TIME_SERIES_AGG)).isSelected();
			boolean displayExceedanceAll = ((JCheckBox) _swingEngine.find(EXCEEDANCE_ALL)).isSelected();
			boolean displayExceedanceAggregate = ((JCheckBox) _swingEngine.find(EXCEEDANCE_AGG)).isSelected();
			boolean displayBoxAndWhiskerAll = ((JCheckBox) _swingEngine.find(BOX_ALL)).isSelected();
			boolean displayBoxAndWhiskerAggregate = ((JCheckBox) _swingEngine.find(BOX_AGG)).isSelected();
			boolean displayMonthlyTable = ((JCheckBox) _swingEngine.find(MONTHLY_TABLE)).isSelected();
			boolean displaySummaryTable = ((JCheckBox) _swingEngine.find(SUMMARY_TABLE)).isSelected();
			boolean displayMonthlyLine = ((JCheckBox) _swingEngine.find(MONTHLY_LINE)).isSelected();
			boolean displayBarCharts = ((JCheckBox) _swingEngine.find(BAR_GRAPH)).isSelected();
			return new PlotConfigurationState(displayTimeSeriesAll, displayTimeSeriesAggregate, displayExceedanceAll,
					displayExceedanceAggregate,
					displayBoxAndWhiskerAll, displayBoxAndWhiskerAggregate, displayMonthlyTable, displaySummaryTable, displayMonthlyLine,
					displayBarCharts);
		}
		catch(RuntimeException e)
		{
			throw new EpptPlotException("Error building plot configuration state", e);
		}
	}

	public static PlotConfigurationState fromString(String displayGroup)
	{
		boolean displayTimeSeriesAll = displayGroup.contains(TIME_SERIES_ALL);
		boolean displayTimeSeriesAggregate = displayGroup.contains(TIME_SERIES_AGG);
		boolean displayExceedanceAll = displayGroup.contains(EXCEEDANCE_ALL);
		boolean displayExceedanceAggregate = displayGroup.contains(EXCEEDANCE_AGG);
		boolean displayBoxAndWhiskerAll = displayGroup.contains(BOX_ALL);
		boolean displayBoxAndWhiskerAggregate = displayGroup.contains(BOX_AGG);
		boolean displayMonthlyTable = displayGroup.contains(MONTHLY_TABLE);
		boolean displaySummaryTable = displayGroup.contains(SUMMARY_TABLE);
		boolean displayMonthlyLine = displayGroup.contains(MONTHLY_LINE);
		boolean displayBarCharts = displayGroup.contains(BAR_GRAPH);
		return new PlotConfigurationState(displayTimeSeriesAll, displayTimeSeriesAggregate, displayExceedanceAll,
				displayExceedanceAggregate, displayBoxAndWhiskerAll, displayBoxAndWhiskerAggregate,
				displayMonthlyTable, displaySummaryTable, displayMonthlyLine, displayBarCharts);
	}
}

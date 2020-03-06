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
	private final SwingEngine _swingEngine;

	public PlotConfigurationStateBuilder(SwingEngine swingEngine)
	{
		_swingEngine = swingEngine;
	}

	public String createQuickStateString() throws EpptPlotException
	{
		return "";
	}

	public PlotConfigurationState createPlotConfigurationState() throws EpptPlotException
	{
		try
		{
			// Base, Comparison and Difference
			boolean displayTimeSeriesAll = ((JCheckBox) _swingEngine.find("TimeSeriesAll")).isSelected();
			boolean displayTimeSeriesAggregate = ((JCheckBox) _swingEngine.find("TimeSeriesAggregate")).isSelected();
			boolean displayExceedanceAll = ((JCheckBox) _swingEngine.find("ExceedanceAll")).isSelected();
			boolean displayExceedanceAggregate = ((JCheckBox) _swingEngine.find("ExceedanceAggregate")).isSelected();
			boolean displayBoxAndWhiskerAll = ((JCheckBox) _swingEngine.find("BoxAndWhiskerAll")).isSelected();
			boolean displayBoxAndWhiskerAggregate = ((JCheckBox) _swingEngine.find("BoxAndWhiskerAggregate")).isSelected();
			boolean displayMonthlyTable = ((JCheckBox) _swingEngine.find("MonthlyTable")).isSelected();
			boolean displaySummaryTable = ((JCheckBox) _swingEngine.find("SummaryTable")).isSelected();
			boolean displayMonthlyLine = ((JCheckBox) _swingEngine.find("MonthlyLine")).isSelected();
			boolean displayBarCharts = ((JCheckBox) _swingEngine.find("BarGraph")).isSelected();
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
		return new PlotConfigurationState(false, false, false, false, false, false, false, false, false, false);
	}
}

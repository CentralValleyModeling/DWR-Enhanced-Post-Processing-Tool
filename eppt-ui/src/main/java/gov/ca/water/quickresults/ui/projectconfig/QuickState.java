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

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

import org.jfree.data.time.Month;

public class QuickState
{

	private static final String[] MONTH_NAMES = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
			"Dec"};

	private final ComparisonType _comparisonType;
	private final boolean _isDisplayCFS;
	private final boolean _isDisplayTimeSeriesPlot;
	private final boolean _isDisplayBoxAndWhiskerPlot;
	private final Month _startMonth;
	private final Month _endMonth;
	private final boolean _isDisplayMonthlyTable;
	private final boolean _isDisplaySummaryTable;
	private final List<Component> _exceedancePlotComponents;
	private final List<Component> _summaryTableComponents;

	public QuickState(ComparisonType comparisonType, boolean isDisplayCFS, boolean isDisplayTimeSeriesPlot, boolean isDisplayBoxAndWhiskerPlot,
					  Month startMonthAndYear, Month endMonthAndYear, List<Component> exceedancePlotComponents, boolean isDisplayMonthlyTable,
					  boolean isDisplaySummaryTable, List<Component> summaryTableComponents)
	{
		_comparisonType = comparisonType;
		_isDisplayCFS = isDisplayCFS;
		_isDisplayTimeSeriesPlot = isDisplayTimeSeriesPlot;
		_isDisplayBoxAndWhiskerPlot = isDisplayBoxAndWhiskerPlot;
		_startMonth = startMonthAndYear;
		_endMonth = endMonthAndYear;

		_exceedancePlotComponents = exceedancePlotComponents;

		_isDisplayMonthlyTable = isDisplayMonthlyTable;
		_isDisplaySummaryTable = isDisplaySummaryTable;

		_summaryTableComponents = summaryTableComponents;
	}

	public ComparisonType getComparisonType()
	{
		return _comparisonType;
	}

	public boolean isDisplayCFS()
	{
		return _isDisplayCFS;

	}

	public boolean isDisplayTimeSeriesPlot()
	{
		return _isDisplayTimeSeriesPlot;
	}

	public boolean isDisplayBoxAndWhiskerPlot()
	{
		return _isDisplayBoxAndWhiskerPlot;
	}

	public String getStartMonth()
	{
		int startMonthInt = _startMonth.getMonth();
		switch(startMonthInt)
		{
			case 1:
				return "Jan";
			case 2:
				return "Feb";
			case 3:
				return "Mar";
			case 4:
				return "Apr";
			case 5:
				return "May";
			case 6:
				return "Jun";
			case 7:
				return "Jul";
			case 8:
				return "Aug";
			case 9:
				return "Sep";
			case 10:
				return "Oct";
			case 11:
				return "Nov";
			case 12:
				return "Dec";
			default:
				return "";

		}
	}

	public int getStartYear()
	{
		return _startMonth.getYearValue();
	}

	public String getEndMonth()
	{
		int endMonthInt = _endMonth.getMonth();
		return MONTH_NAMES[endMonthInt];

	}

	public int getEndYear()
	{
		return _endMonth.getYearValue();
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
		for(Component component : _exceedancePlotComponents)
		{
			if(component instanceof JCheckBox)
			{
				JCheckBox c = (JCheckBox) component;
				String cName = c.getText();
				if("ALL".equals(cName))
				{
					return c.isSelected();
				}
			}
		}
		return false;
	}

	public boolean isAnnualFlowExceedancePlots()
	{
		for(Component component : _exceedancePlotComponents)
		{
			if(component instanceof JCheckBox)
			{
				JCheckBox c = (JCheckBox) component;
				String cName = c.getText();
				if("Annual Flow".equals(cName))
				{
					return c.isSelected();
				}
			}
		}
		return false;
	}

	public List<Component> getSelectedExceedancePlotMonths()
	{
		List<Component> selectedMonths = new ArrayList<>();
		for(Component component : _exceedancePlotComponents)
		{
			if(component instanceof JCheckBox)
			{
				JCheckBox c = (JCheckBox) component;
				String cName = c.getText();
				if("Annual Flow".equals(cName) || "ALL".equals(cName))
				{
					continue;
				}
				else if(c.isSelected())
				{
					selectedMonths.add(c);
				}
			}
		}
		return selectedMonths;
	}

	public List<Component> getSelectedSummaryTableItems()
	{
		List<Component> selectedSummaryTable = new ArrayList<>();
		for(Component component : _summaryTableComponents)
		{
			if(component instanceof JCheckBox)
			{
				JCheckBox c = (JCheckBox) component;
				if(c.isSelected())
				{
					selectedSummaryTable.add(c);
				}
			}
		}
		return selectedSummaryTable;
	}

	public enum ComparisonType
	{
		BASE, COMP, DIFF
	}


}

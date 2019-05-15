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

import org.jfree.data.time.Month;

public class QuickStateBuilder
{

	QuickState.ComparisonType compType;
	boolean isDisplayCFS;
	boolean isDisplayTimeSeriesPlot;
	boolean isDisplayBoxAndWhiskerPlot;
	Month startMonthAndYear;

	public QuickStateBuilder()
	{

	}

	public QuickStateBuilder withComparisonType(QuickState.ComparisonType compType)
	{
		compType = compType;
		return this;
	}

	public QuickStateBuilder withDisplayTimeSeriesPlot()
	{
		return this;

	}

	public QuickStateBuilder withDisplayCFS()
	{
		return this;

	}

	public QuickStateBuilder withDisplayTAF()
	{
		return this;

	}

	public QuickStateBuilder withDisplayBoxAndWhiskerPlot()
	{
		return this;

	}

	public QuickStateBuilder withStartMonth(Month startMonth)
	{
		return this;

	}

	public QuickStateBuilder withEndMonth(Month endMonth)
	{
		return this;

	}

	public QuickStateBuilder withDisplayMonthlyTable()
	{
		return this;

	}

	public QuickStateBuilder withDisplaySummaryTable()
	{
		return this;

	}

	public QuickStateBuilder withPlotAllExceedancePlots()
	{
		return this;

	}

	public QuickStateBuilder withAnnualExceedancePlots()
	{
		return this;

	}

	public QuickStateBuilder withExceedancePlots(Component[] components)
	{
		return this;

	}

	public QuickState createQuickState()
	{
		return null;
		//return new QuickState();
	}


}

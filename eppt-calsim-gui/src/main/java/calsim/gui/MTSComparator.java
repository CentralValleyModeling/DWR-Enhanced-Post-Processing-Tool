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

package calsim.gui;

import java.util.Comparator;

import calsim.app.MultipleTimeSeries;

/**
 * A class to compare multiple time series by name
 *
 * @author Nicky Sandhu
 * @version $Id: MTSComparator.java,v 1.1.2.2 2000/12/20 20:07:18 amunevar Exp $
 */
public class MTSComparator implements Comparator
{
	public int compare(Object obj1, Object obj2)
	{
		if(obj1 == null)
		{
			return -1;
		}
		if(obj2 == null)
		{
			return 1;
		}
		if(!(obj1 instanceof MultipleTimeSeries))
		{
			return -1;
		}
		if(!(obj2 instanceof MultipleTimeSeries))
		{
			return 1;
		}
		MultipleTimeSeries dts1 = (MultipleTimeSeries) obj1;
		MultipleTimeSeries dts2 = (MultipleTimeSeries) obj2;
		return dts1.getName().compareTo(dts2.getName());
	}

	public boolean equals(Object obj)
	{
		return false;
	}
}

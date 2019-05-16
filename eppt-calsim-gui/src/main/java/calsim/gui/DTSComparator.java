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

import calsim.app.DerivedTimeSeries;

/**
 * A class to compare derived time series by name
 *
 * @author Nicky Sandhu
 * @version $Id: DTSComparator.java,v 1.1.2.2 2000/12/20 20:07:03 amunevar Exp $
 */
public class DTSComparator implements Comparator
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
		if(!(obj1 instanceof DerivedTimeSeries))
		{
			return -1;
		}
		if(!(obj2 instanceof DerivedTimeSeries))
		{
			return 1;
		}
		DerivedTimeSeries dts1 = (DerivedTimeSeries) obj1;
		DerivedTimeSeries dts2 = (DerivedTimeSeries) obj2;
		return dts1.getName().compareTo(dts2.getName());
	}

	public boolean equals(Object obj)
	{  //CB TODO
		return false;
	}
}

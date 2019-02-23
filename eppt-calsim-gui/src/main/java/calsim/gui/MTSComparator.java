/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

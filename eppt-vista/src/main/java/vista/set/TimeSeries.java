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
package vista.set;

import vista.time.Time;
import vista.time.TimeWindow;

/**
 * Time Series is the base class for time series. There are two kinds of time
 * series, a sequence of regular interval separated values ( RegularTimeSeries )
 * and irregular interval separated values( IrregularTimeSeries).
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: TimeSeries.java,v 1.1 2003/10/02 20:49:33 redwood Exp $
 * @see RegularTimeSeries, IrregularTimeSeries
 */
public abstract class TimeSeries implements DataSet
{
	/**
	 * returns the element at the given time. If no element exists at exactly
	 * the specified time an exception is thrown
	 */
	public abstract TimeElement getElementAt(String tm);

	/**
	 * returns the element at the given time. If no element exists at exactly
	 * the specified time null is returned
	 */
	public abstract TimeElement findElementAt(String tm);

	/**
	 * get start time
	 */
	public abstract Time getStartTime();

	/**
	 * get end time
	 */
	public abstract Time getEndTime();

	/**
	 * get time window
	 */
	public abstract TimeWindow getTimeWindow();

	/**
	 * creates a time window with the intersecting time window
	 */
	public abstract TimeSeries createSlice(TimeWindow tw);

	/**
	 * same as createSlice(TimeWindow tw)
	 *
	 * @see TimeSeries#createSlice(TimeWindow tw)
	 */
	public abstract TimeSeries createSlice(String stime, String etime);
}

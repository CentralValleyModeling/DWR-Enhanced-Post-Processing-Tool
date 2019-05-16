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
package vista.time;

import java.io.Serializable;

/**
 * Defines a range of time. This is used to specify the range for which the data
 * may be retrieved.
 */
public class DefaultTimeWindow implements TimeWindow, Serializable
{
	Time _endTime;
	Time _startTime;

	/**
	 * private constructor
	 */
	DefaultTimeWindow()
	{
	}

	/**
	 * creates a time window using the given start and end minutes since Dec 31,
	 * 1899.
	 */
	DefaultTimeWindow(long stime, long etime)
	{
		if(stime > etime)
		{
			throw new IllegalArgumentException("Start time: " + stime + " > "
					+ "End time: " + etime);
		}
		TimeFactory _tf = TimeFactory.getInstance();
		_startTime = _tf.getTimeInstance().create(stime);
		_endTime = _tf.getTimeInstance().create(etime);
	}

	/**
	 * create a copy of itself
	 *
	 * @return the copy
	 */
	public TimeWindow create()
	{
		DefaultTimeWindow ntw = new DefaultTimeWindow();
		ntw._startTime = _startTime;
		ntw._endTime = _endTime;
		return ntw;
	}

	/**
	 * creates a time window with the given start and end times.
	 */
	public TimeWindow create(Time startTime, Time endTime)
	{
		return new DefaultTimeWindow(startTime.getTimeInMinutes(), endTime
				.getTimeInMinutes());
	}

	/**
	 * Gets the starting time for this window
	 *
	 * @return the starting time since Dec 31, 1899 Midnight
	 */
	public Time getStartTime()
	{
		return _startTime;
	}

	/**
	 * Gets the end time for this window
	 *
	 * @return the ending time since Dec 31, 1899 Midnight
	 */
	public Time getEndTime()
	{
		return _endTime;
	}

	/**
	 * creates a time window which spans both this and the provided time window
	 *
	 * @param timeWindow
	 * @return
	 */
	public TimeWindow union(TimeWindow timeWindow)
	{
		if(timeWindow == null)
		{
			return this.create(this.getStartTime(), this.getEndTime());
		}
		long stime = _startTime.getTimeInMinutes();
		long etime = _endTime.getTimeInMinutes();
		long ostime = timeWindow.getStartTime().getTimeInMinutes();
		long oetime = timeWindow.getEndTime().getTimeInMinutes();
		long nstime = Math.min(stime, ostime);
		long netime = Math.max(etime, oetime);
		if(nstime > netime)
		{
			return null;
		}
		else
		{
			return new DefaultTimeWindow(nstime, netime);
		}
	}

	/**
	 * creates a time window which is the intersection of this time window with
	 * given time window.
	 *
	 * @return new time window object representing intersection or null if no
	 * intersection is possible.
	 */
	public TimeWindow intersection(TimeWindow timeWindow)
	{
		if(timeWindow == null)
		{
			return null;
		}
		long stime = _startTime.getTimeInMinutes();
		long etime = _endTime.getTimeInMinutes();
		long ostime = timeWindow.getStartTime().getTimeInMinutes();
		long oetime = timeWindow.getEndTime().getTimeInMinutes();
		long nstime = Math.max(stime, ostime);
		long netime = Math.min(etime, oetime);
		if(nstime > netime)
		{
			return null;
		}
		else
		{
			return new DefaultTimeWindow(nstime, netime);
		}
	}

	/**
	 * true if time window intersects with the given time window
	 */
	public boolean intersects(TimeWindow timeWindow)
	{
		if(timeWindow == null)
		{
			return false;
		}
		long stime = _startTime.getTimeInMinutes();
		long etime = _endTime.getTimeInMinutes();
		long ostime = timeWindow.getStartTime().getTimeInMinutes();
		long oetime = timeWindow.getEndTime().getTimeInMinutes();
		return (ostime <= etime) && (oetime >= stime);
	}

	/**
	 * returns true if given time window is contained completely in the current
	 * time window.
	 */
	public boolean contains(TimeWindow timeWindow)
	{
		if(timeWindow == null)
		{
			return false;
		}
		long stime = _startTime.getTimeInMinutes();
		long etime = _endTime.getTimeInMinutes();
		long ostime = timeWindow.getStartTime().getTimeInMinutes();
		long oetime = timeWindow.getEndTime().getTimeInMinutes();
		return ((ostime >= stime) && (oetime <= etime))
				|| ((stime >= ostime) && (etime <= oetime));
	}

	/**
	 * returns true if this time window contains the given time
	 */
	public boolean contains(Time time)
	{
		return ((time != null) && (time.compare(_startTime) >= 0 && time
				.compare(_endTime) <= 0));
	}

	/**
	 * equals implementation
	 */
	public boolean equals(Object obj)
	{
		return (obj != null) && (obj instanceof TimeWindow)
				&& (((TimeWindow) obj).isSameAs(this));
	}

	/**
	 * tests for similarity
	 */
	public boolean isSameAs(TimeWindow tw)
	{
		return (tw.getEndTime().getTimeInMinutes() == _endTime
				.getTimeInMinutes())
				&& (tw.getStartTime().getTimeInMinutes() == _startTime
				.getTimeInMinutes());
	}

	/**
	 * string representation of the time window
	 */
	public String toString()
	{
		// TimeFormat formatter = new DefaultTimeFormat("ddMMMyyyy");
		// //DSSSessionBuilder.getTimeFactory().getTimeFormatInstance();
		// StringBuffer buf = new StringBuffer(100);
		// buf.append(_startTime.format(formatter));
		// buf.append(" - ");
		// buf.append(_endTime.format(formatter));
		// return buf.toString();
		StringBuffer buf = new StringBuffer(100);
		buf.append(_startTime.format());
		buf.append(" - ");
		buf.append(_endTime.format());
		return buf.toString();
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.time;

import java.util.Date;

import org.python.core.Py;

/**
 * A factory initialized with instances of classes to be used as prototypes for
 * creating further instances.
 *
 * @author Nicky Sandhu
 * @version $Id: TimeFactory.java,v 1.1 2003/10/02 20:49:36 redwood Exp $
 */
public class TimeFactory implements java.io.Serializable
{
	/**
	 * an instance of the default implemenations
	 */
	private static TimeFactory _instance = new TimeFactory(new DefaultTime(),
			new DefaultTimeInterval(), new DefaultTimeFormat(),
			new DefaultTimeWindow());
	private Time _time;
	private TimeInterval _ti;
	private TimeFormat _format;
	private TimeWindow _tw;

	/**
	 * instantiate the time factory with prototype instances
	 */
	public TimeFactory(Time time, TimeInterval ti, TimeFormat format,
					   TimeWindow tw)
	{
		_time = time;
		_ti = ti;
		_format = format;
		_tw = tw;
	}

	/**
	 * get the default implementation
	 */
	public static TimeFactory getInstance()
	{
		return _instance;
	}

	/**
	 * creates a time window from the given time window and time interval such
	 * that starting and ending times of the window are sane choices.
	 */
	public static TimeWindow createRoundedTimeWindow(TimeWindow tw,
													 TimeInterval ti)
	{
		if(ti == null)
		{
			return tw;
		}
		Time stm = tw.getStartTime().create(tw.getStartTime());
		Time etm = stm.create(tw.getEndTime());
		stm = stm.create(stm.floor(ti));
		etm = stm.create(etm.ceiling(ti));
		return tw.create(stm, etm);
	}

	/**
	 * for python
	 */
	static TimeInterval getTimeIntervalFromString(String tistr)
	{
		TimeInterval ti = null;
		try
		{
			ti = TimeFactory.getInstance().createTimeInterval(tistr);
		}
		catch(Exception e)
		{
			throw Py.TypeError("time interval string expected");
		}
		return ti;
	}

	/**
	 * for python
	 */
	static Time getTimeFromString(String tmstr)
	{
		Time tm = null;
		try
		{
			tm = TimeFactory.getInstance().createTime(tmstr);
		}
		catch(Exception e)
		{
			throw Py.TypeError("time interval string expected");
		}
		return tm;
	}

	/**
	 * get prototype for time
	 *
	 * @return an instance of the time object to be used as a prototype for
	 * creating other time objects using this instance
	 */
	public Time getTimeInstance()
	{
		return _time;
	}

	/**
	 * creates a time object containing the same time as tm object
	 */
	public Time createTime(Time tm)
	{
		return _time.create(tm);
	}

	/**
	 * creates a time object containing the same time as tm object
	 */
	public Time createTime(long tm)
	{
		return _time.create(tm);
	}

	public Time createTime(Date date)
	{
		Time time = _time.create(0);
		time.setDate(date);
		return time;
	}

	/**
	 * creates a time object containing the same time as tm object
	 */
	public Time createTime(String tmstr)
	{
		return _time.create(tmstr);
	}

	/**
	 * creates a time object containing the same time as tm object
	 */
	public Time createTime(String tmstr, String pattern)
	{
		return _time.create(tmstr, pattern);
	}

	/**
	 * creates a time object containing the same time as tm object
	 */
	public Time createTime(String tmstr, String pattern, TimeFormat formatter)
	{
		return _time.create(tmstr, pattern, formatter);
	}

	/**
	 * get prototype for time interval instance
	 *
	 * @return a prototype for time interval
	 */
	public TimeInterval getTimeIntervalInstance()
	{
		return _ti;
	}

	/**
	 * creates a time interval
	 */
	public TimeInterval createTimeInterval(TimeInterval ti)
	{
		return _ti.create(ti);
	}

	/**
	 * creates a time interval
	 */
	public TimeInterval createTimeInterval(int numberOfIntervals,
										   int intervalType)
	{
		return _ti.create(numberOfIntervals, intervalType);
	}

	/**
	 * creates a time interval
	 */
	public TimeInterval createTimeInterval(String intervalStr)
	{
		return _ti.create(intervalStr);
	}

	/**
	 * get prototype for time format
	 *
	 * @return a prototype for time format object
	 */
	public TimeFormat getTimeFormatInstance()
	{
		return _format;
	}

	/**
	 * get instance of time window
	 */
	public TimeWindow getTimeWindowInstance()
	{
		return _tw;
	}

	/**
	 * creates time window with given start and end times
	 */
	public TimeWindow createTimeWindow(Time st, Time et)
	{
		return _tw.create(st, et);
	}

	/**
	 * creates time window from given string of TimeFormat time strings
	 * separated by a dash '-'
	 */
	public TimeWindow createTimeWindow(String s)
	{
		int dIndex = s.indexOf("-");
		if(dIndex < 0)
		{
			throw new IllegalArgumentException(
					"Invalid string for time window " + s);
		}
		String ststr = s.substring(0, dIndex).trim();
		String etstr = s.substring(dIndex + 1).trim();
		Time stime = this.createTime(ststr);
		Time etime = this.createTime(etstr);
		return _tw.create(stime, etime);
	}

	/**
	 * creates time window from given string of TimeFormat time strings
	 * separated by a dash '-'
	 */
	public TimeWindow createTimeWindow(String s, String pattern)
	{
		int dIndex = s.indexOf("-");
		if(dIndex < 0)
		{
			throw new IllegalArgumentException(
					"Invalid string for time window " + s);
		}
		String ststr = s.substring(0, dIndex).trim();
		String etstr = s.substring(dIndex + 1).trim();
		Time stime = this.createTime(ststr, pattern);
		Time etime = this.createTime(etstr, pattern);
		return _tw.create(stime, etime);
	}
}

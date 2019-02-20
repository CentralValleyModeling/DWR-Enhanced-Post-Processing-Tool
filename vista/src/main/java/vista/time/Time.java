/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.time;

import java.util.Date;

/**
 * An interface to a time object.
 *
 * @author Nicky Sandhu
 * @version $Id: Time.java,v 1.1 2003/10/02 20:49:36 redwood Exp $
 */
public interface Time extends java.io.Serializable
{
	/**
	 * creates a copy of the time object
	 *
	 * @return the newly created copy
	 */
	Time create(Time time);

	/**
	 * create a time object
	 *
	 * @param tm The time in minutes since base time.
	 * @return the time object
	 */
	Time create(long tm);

	/**
	 * create a time object using default format and string
	 *
	 * @param tmstr The time string
	 * @return the time object
	 */
	Time create(String tmstr);

	/**
	 * create a time object using default format and string
	 *
	 * @param tmstr The time string
	 * @return the time object
	 */
	Time create(String tmstr, String pattern);

	/**
	 * create a time object initialized with time using time string and the
	 * formatter
	 *
	 * @param tmstr     The time string
	 * @param formatter The formatter for parsing the time string
	 * @return the time object
	 */
	Time create(String tmstr, String pattern, TimeFormat formatter);

	/**
	 * get time in minutes since base time.
	 *
	 * @return the time in minutes
	 */
	long getTimeInMinutes();

	/**
	 * @param time the time object to which this is compared to.
	 * @return 0 if equal, -ve if this time < given time or +ve if this time >
	 * given time
	 */
	int compare(Time time);

	/**
	 * formats date with default date formatter
	 *
	 * @return a string representation of the time.
	 */
	String format();

	/**
	 * formats date with given date formatter
	 *
	 * @param formatter The formatting object
	 * @return a string representation of the time.
	 */
	String format(TimeFormat formatter);

	/**
	 * @param intervalId        The id of the field to be incremented
	 * @param numberOfIntervals The number of intervals of that field to be incremented by.
	 */
	void incrementBy(int intervalId, int numberOfIntervals);

	/**
	 * @param ti The time interval by which current time is incremented by.
	 */
	void incrementBy(TimeInterval ti);

	/**
	 * @param numberOfIncrements the number of increments
	 * @param ti                 The interval to be incremented by
	 */
	void incrementBy(TimeInterval ti, int numberOfIncrements);

	/**
	 * @param the time being compared with
	 * @return the number of minutes to given time
	 */
	long getNumberOfMinutesTo(Time time);

	/**
	 * Converts the time value to date
	 *
	 * @return
	 */
	Date getDate();

	void setDate(Date d1);

	/**
	 * calculate exactly the number of intervals as defined by the interval to
	 * be incremented for this time to be equal to given time.
	 *
	 * @param time The time to which interval is desiredd
	 * @param ti   the time interval by which current time will be incremented
	 *             by.
	 * @return number of times to be incremented by ti to exceed or be equal to
	 * given time
	 */
	long getExactNumberOfIntervalsTo(Time time, TimeInterval ti);

	/**
	 * @param time The time to which interval is desiredd
	 * @param ti   the time interval by which current time will be incremented
	 *             by.
	 * @return number of times to be incremented by ti to exceed or be equal to
	 * given time
	 */
	long getNumberOfIntervalsTo(Time time, TimeInterval ti);

	/**
	 * rounds time to nearest nice interval >= current time and returns a new
	 * time with value
	 */
	Time ceiling(TimeInterval ti);

	/**
	 * rounds time to nearest nice interval =< current time and returns a new
	 * time with value
	 */
	Time floor(TimeInterval ti);

	/**
	 * rounds time to nearest nice interval. If exactly in middle it rounds to
	 * ceiling.
	 */
	Time round(TimeInterval ti);

	/**
	 * for python
	 */
	TimeInterval __sub__(Time tm);

	/**
	 * for python
	 */
	Time __add__(TimeInterval ti);

	/**
	 * for python
	 */
	Time __radd__(TimeInterval ti);

	/**
	 * for python
	 */
	Time __sub__(TimeInterval ti);

	/**
	 * for python
	 */
	Time __rsub__(TimeInterval ti);

	/**
	 * for python
	 */
	Time __add__(String ti);

	/**
	 * for python
	 */
	Time __radd__(String ti);

	/**
	 * for python
	 */
	Object __sub__(String ti);

	/**
	 * for python
	 */
	Object __rsub__(String ti);
}

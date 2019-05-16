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
import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

import org.python.core.Py;

/**
 * A class representing time as used within the DSS database.
 *
 * @author Nicky Sandhu
 * @version $Id: DefaultTime.java,v 1.1 2003/10/02 20:49:35 redwood Exp $
 */
public class DefaultTime implements Time, Serializable
{
	/**
	 * the default pattern for interpreting a time string.
	 */
	public static final String DEFAULT_PATTERN = "ddMMMyyyy HHmm";
	/**
	 * for saving creation cost
	 */
	private static StringBuffer _buf = new StringBuffer(20);
	private static TimeFormat _dtf = new DefaultTimeFormat("ddMMMyyyy HHmm",
			Locale.US);
	private static DefaultTime _ceiling = (DefaultTime) new DefaultTime()
			.create("01JAN1990 0000", "ddMMMyyyy HHmm", _dtf),
			_floor = (DefaultTime) new DefaultTime().create("01JAN1990 0000",
					"ddMMMyyyy HHmm", _dtf),
			_rounded = (DefaultTime) new DefaultTime().create("01JAN1990 0000",
					"ddMMMyyyy HHmm", _dtf);
	/**
	 * The instance of gregorian calendar
	 */
	private static Calendar _calendar;
	/**
	 * The beginning time in millisecs as defined by Dec 31 1899 2400. This is
	 * the number of millisecs since Jan 01, 1970, 0000
	 */
	private static long _time0;

	/**
	 * sets the base time as midnight Dec 31, 1899
	 */
	static
	{
		TimeZone tz = TimeZone.getTimeZone("GMT");
		_calendar = Calendar.getInstance(tz);
		// _calendar.set(1900,0,1, 0, 0, 0);
		_calendar.set(1899, 11, 31, 0, 0, 0);
		Date date0 = _calendar.getTime();
		_time0 = date0.getTime();
	}

	/**
	 * formats date according to specifications
	 */
	private DateFormat _formatter;
	/**
	 * The underlying date representing this time object
	 */
	private Date _date;

	/**
	 * Initializes the start time as 1899, Dec, 31.
	 */
	DefaultTime()
	{
		_formatter = _dtf;// new DefaultTimeFormat("ddMMMyyyy HHmm", Locale.US);
	}

	/**
	 * creates a new object by parsing time string using the pattern defined in
	 * following string. This pattern is along the same lines as that of
	 * java.util.DateFormat.
	 */
	private static DefaultTime createDefaultTime(String timeString,
												 String pattern) throws IllegalArgumentException
	{
		DefaultTime time = new DefaultTime();
		try
		{
			time._formatter = new DefaultTimeFormat(pattern, Locale.US);
			time.setTime(timeString);
		}
		catch(ParseException pe)
		{
			throw new IllegalArgumentException("TimeString: " + timeString
					+ " does not match pattern: " + pattern);
		}
		return time;
	}

	/**
	 * creates a new object by parsing time string using the pattern defined in
	 * following string. This pattern is along the same lines as that of
	 * java.util.DateFormat.
	 */
	private static DefaultTime createDefaultTime(String timeString)
			throws IllegalArgumentException
	{
		return createDefaultTime(timeString, "ddMMMyyyy HHmm");
	}

	/**
	 * returns a DefaultTime object for the given time in minutes since Dec 31,
	 * 1899.
	 */
	private static DefaultTime createDefaultTime(long julianMins)
	{
		DefaultTime time = new DefaultTime();
		time.setJulianMins(julianMins);
		return time;
	}

	/**
	 * creates a time object which is the same time as the given object.
	 */
	private static DefaultTime createDefaultTime(DefaultTime time)
	{
		return createDefaultTime(time.getJulianMins());
	}

	/**
	 * creates a new time by incrementing amount on field. The field identifier
	 * is the same as defined in Calendar class
	 *
	 * @see java.util.Calendar
	 */
	private static DefaultTime createDefaultTime(DefaultTime time, int field,
												 int amount) throws IllegalArgumentException
	{
		DefaultTime newTime = createDefaultTime(time);
		_calendar.setTime(newTime._date);
		_calendar.add(field, amount);
		newTime._date = _calendar.getTime();
		return newTime;
	}

	/**
	 * creates a new time by rolling amount on field if roll is true else it
	 * just increments by amount on field The field identifier is the same as
	 * defined in Calendar class
	 *
	 * @see java.util.Calendar
	 */
	private static DefaultTime createDefaultTime(DefaultTime time, int field,
												 int amount, boolean roll) throws IllegalArgumentException
	{
		DefaultTime newTime = createDefaultTime(time);
		if(roll)
		{
			_calendar.setTime(newTime._date);
			boolean rollUp = true;
			if(amount < 0)
			{
				rollUp = false;
			}
			for(int i = 0; i < Math.abs(amount); i++)
			{
				_calendar.roll(field, rollUp);
			}
			newTime._date = _calendar.getTime();
		}
		else
		{
			_calendar.setTime(newTime._date);
			_calendar.add(field, amount);
			newTime._date = _calendar.getTime();
		}
		return newTime;
	}

	/**
	 * creates a time which is an increment over given time as defined by the
	 * increment string. increment is given by a quantity and string. E.g.
	 * 1HOUR, -3min, 30days, 3months, 2years etcetra.
	 */
	private static DefaultTime createDefaultTime(DefaultTime time,
												 String increment) throws IllegalArgumentException
	{
		int index = -1;
		increment = increment.toUpperCase();
		if((index = increment.indexOf(TimeInterval.MIN_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.MIN_INTERVAL_STR);
			return createDefaultTime(time.getJulianMins() + nIntervals);
		}
		else if((index = increment.indexOf(TimeInterval.HOUR_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.HOUR_INTERVAL_STR);
			return createDefaultTime(time.getJulianMins() + nIntervals * 60);
		}
		else if((index = increment.indexOf(TimeInterval.DAY_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.DAY_INTERVAL_STR);
			return createDefaultTime(time.getJulianMins() + nIntervals * 1440);
		}
		else if((index = increment.indexOf(TimeInterval.MONTH_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.MONTH_INTERVAL_STR);
			DefaultTime newTime = createDefaultTime(time, Calendar.MONTH,
					nIntervals);
			return newTime;
		}
		else if((index = increment.indexOf(TimeInterval.YEAR_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.YEAR_INTERVAL_STR);
			DefaultTime newTime = createDefaultTime(time, Calendar.YEAR,
					nIntervals);
			return newTime;
		}
		else
		{
			throw new IllegalArgumentException("increment : " + increment
					+ " not understood");
		}
	}

	/**
	 * creates a time which is an increment over given time as defined by the
	 * increment string. increment is given by a quantity and string. E.g.
	 * 1HOUR, -3min, 30days, 3months, 2years etcetra.
	 */
	private static DefaultTime createDefaultTime(DefaultTime time,
												 String increment, boolean roll) throws IllegalArgumentException
	{
		int index = -1;
		increment = increment.toUpperCase();
		if((index = increment.indexOf(TimeInterval.MIN_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.MIN_INTERVAL_STR);
			return createDefaultTime(time.getJulianMins() + nIntervals);
		}
		else if((index = increment.indexOf(TimeInterval.HOUR_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.HOUR_INTERVAL_STR);
			return createDefaultTime(time.getJulianMins() + nIntervals * 60);
		}
		else if((index = increment.indexOf(TimeInterval.DAY_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.DAY_INTERVAL_STR);
			return createDefaultTime(time.getJulianMins() + nIntervals * 1440);
		}
		else if((index = increment.indexOf(TimeInterval.MONTH_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.MONTH_INTERVAL_STR);
			DefaultTime newTime = createDefaultTime(time, Calendar.MONTH,
					nIntervals, roll);
			return newTime;
		}
		else if((index = increment.indexOf(TimeInterval.YEAR_INTERVAL_STR)) >= 0)
		{
			int nIntervals = getNumberOfIntervals(increment,
					TimeInterval.YEAR_INTERVAL_STR);
			DefaultTime newTime = createDefaultTime(time, Calendar.YEAR,
					nIntervals, roll);
			return newTime;
		}
		else
		{
			throw new IllegalArgumentException("increment : " + increment
					+ " not understood");
		}
	}

	/**
	 * return number of intervals of unit substring in string. e.g. 3hour has 3
	 * intervals of unit hour
	 */
	private static int getNumberOfIntervals(String string, String substring)
	{
		int index = string.indexOf(substring);
		if(index < 0)
		{
			return 0;
		}
		String nIntervals = string.substring(0, index);
		return new Integer(nIntervals).intValue();
	}

	/**
	 * gets the time as minutes since Dec 31, 1899
	 */
	private long getJulianMins()
	{
		return getTimeInMinutes();
	}

	/**
	 * sets the time as minutes since Dec 31, 1899
	 */
	private void setJulianMins(long julianMins)
	{
		_date = new Date(julianMins * 60000 + _time0);
	}

	/**
	 * @return time in minutes since Dec 31, 1899 2400.
	 */
	public long getTimeInMinutes()
	{
		return Math.round((_date.getTime() - _time0) / 60000.0);
	}

	public Date getDate()
	{
		return new Date(_date.getTime() + _date.getTimezoneOffset() * 60000);
	}

	/**
	 * Creates a new date in GMT Time based on this date
	 *
	 * @param date
	 */
	@SuppressWarnings("deprecation")
	public void setDate(Date date)
	{
		_date = new Date(date.getTime() - date.getTimezoneOffset() * 60000);
	}

	/**
	 * creates a copy of the time object
	 *
	 * @return the newly created copy
	 */
	public Time create(Time time)
	{
		return create(time.getTimeInMinutes());
	}

	/**
	 * create a time object
	 *
	 * @param tm The time in minutes since base time.
	 * @return the time object
	 */
	public Time create(long tm)
	{
		DefaultTime time = new DefaultTime();
		time.setJulianMins(tm);
		return time;
	}

	/**
	 * create a time object using default format and string
	 *
	 * @param tmstr The time string
	 * @return the time object
	 */
	public Time create(String tmstr)
	{
		return create(tmstr, null, null);
	}

	/**
	 * create a time object using default format and string
	 *
	 * @param tmstr The time string
	 * @return the time object
	 */
	public Time create(String tmstr, String pattern)
	{
		return create(tmstr, pattern, null);
	}

	/**
	 * create a time object initialized with time using time string and the
	 * formatter
	 *
	 * @param tmstr     The time string
	 * @param formatter The formatter for parsing the time string
	 * @return the time object
	 */
	public Time create(String tmstr, String pattern, TimeFormat formatter)
	{
		DefaultTime time = new DefaultTime();
		try
		{
			if(pattern == null)
			{
				pattern = DEFAULT_PATTERN;
			}
			if(formatter == null)
			{
				time._formatter = new DefaultTimeFormat(pattern, Locale.US);
			}
			else
			{
				time._formatter = formatter;
			}
			time.setTime(tmstr);
		}
		catch(ParseException pe)
		{
			throw new IllegalArgumentException("TimeString: " + tmstr
					+ " does not match pattern: " + pattern);
		}
		return time;
	}

	/**
	 * @return 0 if equal, -ve if this time < given time or +ve if this time >
	 * given time
	 */
	public int compare(Time time)
	{
		return (int) (getTimeInMinutes() - time.getTimeInMinutes());
	}

	/**
	 * formats date with default date formatter
	 *
	 * @return a string representation of the time.
	 */
	public String format()
	{
		return format(null);
	}

	/**
	 * formats date with given date formatter
	 *
	 * @param formatter The formatting object
	 * @return a string representation of the time.
	 */
	public String format(TimeFormat formatter)
	{
		return formatter == null ? _formatter.format(_date).toUpperCase()
				: formatter.format(_date).toUpperCase();
	}

	/**
	 * returns the number of minutes since midnight.
	 */
	public int getMinutesSinceMidnight()
	{
		return (int) (getJulianMins() % 1440);
	}

	/**
	 * @param intervalId        The id of the field to be incremented
	 * @param numberOfIntervals The number of intervals of that field to be incremented by.
	 */
	public void incrementBy(int intervalId, int numberOfIntervals)
	{
		if(intervalId == TimeInterval.DECADE_INTERVAL)
		{
			numberOfIntervals *= 10;
		}
		if(intervalId == TimeInterval.CENTURY_INTERVAL)
		{
			numberOfIntervals *= 100;
		}
		int amount = numberOfIntervals;
		int field = getDateClassField(intervalId);
		_calendar.setTime(_date);
		_calendar.add(field, amount);
		_date = _calendar.getTime();
	}

	/**
	 *
	 */
	private int getDateClassField(int id)
	{
		switch(id)
		{
			case TimeInterval.MIN_INTERVAL:
				return Calendar.MINUTE;
			case TimeInterval.HOUR_INTERVAL:
				return Calendar.HOUR_OF_DAY;
			case TimeInterval.DAY_INTERVAL:
				return Calendar.DAY_OF_MONTH;
			case TimeInterval.WEEK_INTERVAL:
				return Calendar.WEEK_OF_MONTH;
			case TimeInterval.MONTH_INTERVAL:
				return Calendar.MONTH;
			case TimeInterval.YEAR_INTERVAL:
				return Calendar.YEAR;
			case TimeInterval.DECADE_INTERVAL:
				return Calendar.YEAR;
			case TimeInterval.CENTURY_INTERVAL:
				return Calendar.YEAR;
			default:
				return Calendar.MINUTE;
		}
	}

	/**
	 * @param ti The time interval by which current time is incremented by.
	 */
	public void incrementBy(TimeInterval ti)
	{
		long incr = ti.getIntervalInMinutes(this);
		setJulianMins(getTimeInMinutes() + incr);
	}

	/**
	 * @param numberOfIncrements the number of increments
	 * @param ti                 The interval to be incremented by
	 */
	public void incrementBy(TimeInterval ti, int numberOfIncrements)
	{
		// Warning: Don't take short cut of multiply number of increments
		// and the number of minutes in the interval.
		if(numberOfIncrements < 0)
		{
			ti = ti.createByMultiplying(-1);
			numberOfIncrements = -numberOfIncrements;
		}
		//
		if(ti.isTimeContextDependent())
		{
			for(int i = 0; i < numberOfIncrements; i++)
			{
				incrementBy(ti);
			}
		}
		else
		{
			long incr = ti.getIntervalInMinutes(this) * numberOfIncrements;
			setJulianMins(getTimeInMinutes() + incr);
		}
	}

	/**
	 * @param the time being compared with
	 * @return the number of minutes to given time
	 */
	public long getNumberOfMinutesTo(Time time)
	{
		return time.getTimeInMinutes() - getTimeInMinutes();
	}

	/**
	 * @param time The time to which interval is desired
	 * @param ti   the time interval by which current time will be incremented
	 *             by.
	 * @return number of times to be incremented by ti to exceed or be equal to
	 * given time
	 */
	public long getExactNumberOfIntervalsTo(Time time, TimeInterval ti)
	{
		Time stime = create(this);
		Time etime = create(time);
		long mins = ti.getIntervalInMinutes(this);
		long intervals = 0;
		int multp = 1;
		if(ti.isTimeContextDependent())
		{
			if(stime.compare(etime) > 0)
			{
				if(mins > 0)
				{
					ti = ti.createByMultiplying(-1);
					multp = -1;
				}
				while(stime.compare(etime) > 0)
				{
					stime.incrementBy(ti);
					intervals++;
				}
			}
			else if(stime.compare(etime) < 0)
			{
				if(mins < 0)
				{
					ti = ti.createByMultiplying(-1);
					multp = -1;
				}
				while(stime.compare(etime) < 0)
				{
					stime.incrementBy(ti);
					intervals++;
				}
			}
			else
			{ // times are equal
			}
			if(stime.compare(etime) != 0)
			{
				throw new RuntimeException("Not possible to increment " + this
						+ " by " + ti + " to exactly reach " + etime);
			}
		}
		else
		{
			long sm = stime.getTimeInMinutes();
			long em = etime.getTimeInMinutes();
			intervals = (em - sm) / mins;
			multp = 1;
			if(em != (sm + mins * intervals))
			{
				throw new RuntimeException("Not possible to increment " + this
						+ " by " + ti + " to exactly reach " + etime);
			}
		}
		return multp * intervals;
	}

	/**
	 * The number of times to be incremented by time interval type to exceed or
	 * be equal to given time.
	 */
	public long getNumberOfIntervalsTo(Time time, TimeInterval ti)
	{
		Time stime = create(this);
		Time etime = create(time);
		long mins = ti.getIntervalInMinutes(this);
		if(stime.compare(etime) > 0)
		{
			if(mins > 0)
			{
				throw new RuntimeException("Cannot increment " + stime + " by "
						+ ti + " to reach " + etime);
			}
			long intervals = stime.getNumberOfMinutesTo(etime) / mins;
			stime.incrementBy(ti, (int) intervals);
			while(stime.compare(etime) > 0)
			{
				stime.incrementBy(ti);
				intervals++;
			}
			return intervals;
		}
		else if(stime.compare(etime) < 0)
		{
			if(mins < 0)
			{
				throw new RuntimeException("Cannot increment " + stime + " by "
						+ ti + " to reach " + etime);
			}
			long intervals = stime.getNumberOfMinutesTo(etime) / mins;
			stime.incrementBy(ti, (int) intervals);
			while(stime.compare(etime) < 0)
			{
				stime.incrementBy(ti);
				intervals++;
			}
			return intervals;
		}
		else
		{ // times are equal
			return 0;
		}
	}

	/**
	 * true if this time is less than equal to given time
	 */
	private boolean lessThanEqualTo(DefaultTime time)
	{
		return (getJulianMins() <= time.getJulianMins());
	}

	/**
	 * true if this time is less than given time
	 */
	private boolean lessThan(DefaultTime time)
	{
		return (getJulianMins() < time.getJulianMins());
	}

	/**
	 * true if this time is greater than equal to given time
	 */
	private boolean greaterThanEqualTo(DefaultTime time)
	{
		return (getJulianMins() >= time.getJulianMins());
	}

	/**
	 * true if this time is greater than given time
	 */
	private boolean greaterThan(DefaultTime time)
	{
		return (getJulianMins() > time.getJulianMins());
	}

	/**
	 * true if this time is the same as given time.
	 */
	private boolean equalTo(DefaultTime time)
	{
		return (getJulianMins() == time.getJulianMins());
	}

	/**
	 * -ve distance from time if less than given time 0 if equal to given time
	 * +ve distance from time if greater than given time
	 */
	private long compareTo(DefaultTime time)
	{
		return getJulianMins() - time.getJulianMins();
	}

	/**
	 * true if object equals this one.
	 */
	public boolean equals(Object obj)
	{
		return (obj != null && obj instanceof DefaultTime && this
				.equalTo((DefaultTime) obj));
	}

	/**
	 * returns a string representation of the string.
	 */
	public String toString(DateFormat formatter)
	{
		return formatter.format(_date).toUpperCase();
	}

	/**
	 * string representation of this object
	 */
	public String toString()
	{
		return _formatter.format(_date).toUpperCase();
	}

	/**
	 * sets time by parsing given string using the pattern as specified in the
	 * static constructor getTime(...,...);
	 */
	private void setTime(String timeString) throws ParseException
	{
		_date = _formatter.parse(timeString);
	}

	/**
	 * increments date by using amount with respect to a certain field.
	 */
	private void rollBy(int field, int amount)
	{
		_calendar.setTime(_date);
		boolean rollUp = true;
		if(amount < 0)
		{
			rollUp = false;
		}
		for(int i = 0; i < Math.abs(amount); i++)
		{
			_calendar.roll(field, rollUp);
		}
		_date = _calendar.getTime();
	}

	/**
	 * rounds time to nearest nice interval >= current time
	 */
	public Time ceiling(TimeInterval ti)
	{
		if(ti == null)
		{
			throw new RuntimeException(
					"Attempt to get the ceiling of a time with a null time interval");
		}
		int field = getLargestIntervalField(ti);
		TimeFormat tf = _dtf;
		String timeStr = getRoundedString(field);
		try
		{
			_ceiling.setTime(timeStr);
		}
		catch(ParseException pe)
		{
			System.out.println("Exception " + pe.getMessage()
					+ " creating ceiling for " + this);
		}
		Time ntm = _ceiling; // create(timeStr, "ddMMMyyyy HHmm", _dtf);
		while(ntm.compare(this) < 0)
		{
			ntm.incrementBy(ti);
		}
		return ntm;
	}

	/**
	 * rounds time to nearest nice interval =< current time
	 */
	public Time floor(TimeInterval ti)
	{
		if(ti == null)
		{
			throw new RuntimeException(
					"Attempt to get the floor of a time with a null time interval");
		}
		int field = getLargestIntervalField(ti);
		String timeStr = getRoundedString(field);
		try
		{
			_floor.setTime(timeStr);
		}
		catch(ParseException pe)
		{
			System.out.println("Exception " + pe.getMessage()
					+ " creating floor for " + this);
		}
		Time ntm = _floor; // create(timeStr, "ddMMMyyyy HHmm", _dtf);
		while(ntm.compare(this) < 0)
		{
			ntm.incrementBy(ti);
		}
		if(ntm.compare(this) != 0)
		{
			ntm.incrementBy(ti, -1);
		}
		return ntm;
	}

	/**
	 * rounds time to nearest nice interval. If exactly in middle it rounds to
	 * ceiling.
	 */
	public Time round(TimeInterval ti)
	{
		if(ti == null)
		{
			throw new RuntimeException(
					"Attempt to round a time with a null time interval");
		}
		int field = getLargestIntervalField(ti);
		TimeFormat tf = _dtf;
		String timeStr = getRoundedString(field);
		try
		{
			_rounded.setTime(timeStr);
		}
		catch(ParseException pe)
		{
			System.out.println("Exception " + pe.getMessage()
					+ " creating round for " + this);
		}
		Time ntm = _rounded;
		while(ntm.compare(this) < 0)
		{
			ntm.incrementBy(ti);
		}
		int distanceFromCeiling = ntm.compare(this), distanceFromFloor = 0;
		if(distanceFromCeiling != 0)
		{
			ntm.incrementBy(ti, -1);
			distanceFromFloor = this.compare(ntm);
			if(distanceFromCeiling < distanceFromFloor)
			{
				ntm.incrementBy(ti, 1);
			}
		}
		else
		{ // if exactly on return this value
		}
		return ntm.create(ntm);
	}

	/**
	 *
	 */
	private String getRoundedString(int field)
	{
		StringBuffer buf = _buf;
		TimeFormat tf = _dtf;
		buf.setLength(0);
		switch(field)
		{
			case TimeInterval.MIN_INTERVAL:
			case TimeInterval.HOUR_INTERVAL:
				tf.format(this._date, buf, new FieldPosition(0));
				buf.setCharAt((12), '0');
				buf.setCharAt((13), '0');
				break;
			case TimeInterval.DAY_INTERVAL:
			case TimeInterval.WEEK_INTERVAL:
				tf.format(this._date, buf, new FieldPosition(0));
				buf.setCharAt((12), '0');
				buf.setCharAt((13), '0');
				buf.setCharAt((10), '0');
				buf.setCharAt((11), '0');
				break;
			case TimeInterval.MONTH_INTERVAL:
				tf.format(this._date, buf, new FieldPosition(0));
				buf.setCharAt((12), '0');
				buf.setCharAt((13), '0');
				buf.setCharAt((10), '0');
				buf.setCharAt((11), '0');
				buf.setCharAt((0), '0');
				buf.setCharAt((1), '1');
				break;
			case TimeInterval.YEAR_INTERVAL:
				tf.format(this._date, buf, new FieldPosition(0));
				buf.setCharAt((12), '0');
				buf.setCharAt((13), '0');
				buf.setCharAt((10), '0');
				buf.setCharAt((11), '0');
				buf.setCharAt((0), '0');
				buf.setCharAt((1), '1');
				buf.setCharAt((2), 'J');
				buf.setCharAt((3), 'A');
				buf.setCharAt((4), 'N');
				break;
			case TimeInterval.DECADE_INTERVAL:
				tf.format(this._date, buf, new FieldPosition(0));
				buf.setCharAt((12), '0');
				buf.setCharAt((13), '0');
				buf.setCharAt((10), '0');
				buf.setCharAt((11), '0');
				buf.setCharAt((0), '0');
				buf.setCharAt((1), '1');
				buf.setCharAt((2), 'J');
				buf.setCharAt((3), 'A');
				buf.setCharAt((4), 'N');
				buf.setCharAt((8), '0');
				break;
			case TimeInterval.CENTURY_INTERVAL:
				tf.format(this._date, buf, new FieldPosition(0));
				buf.setCharAt((12), '0');
				buf.setCharAt((13), '0');
				buf.setCharAt((10), '0');
				buf.setCharAt((11), '0');
				buf.setCharAt((0), '0');
				buf.setCharAt((1), '1');
				buf.setCharAt((2), 'J');
				buf.setCharAt((3), 'A');
				buf.setCharAt((4), 'N');
				buf.setCharAt((7), '0');
				buf.setCharAt((8), '0');
				break;
			default:
				throw new IllegalArgumentException("Time " + this
						+ " has unknown field");
		}
		return buf.toString().toUpperCase();
	}

	/**
	 * returns the largest interval field with a non-zero value
	 */
	private int getLargestIntervalField(TimeInterval ti)
	{
		int field = 1;
		for(int i = 1; i <= TimeInterval.CENTURY_INTERVAL; i++)
		{
			if(ti.getNumberOfIntervals(i) != 0)
			{
				field = i;
			}
		}
		return field;
	}

	/**
	 *
	 */
	public int __cmp__(Object other)
	{
		if(other instanceof Time || other instanceof String)
		{
			if(other instanceof String)
			{
				try
				{
					other = TimeFactory.getTimeFromString((String) other);
				}
				catch(Exception e)
				{
					throw Py.TypeError("time string expected");
				}
			}
			int val = this.compare((Time) other);
			if(val > 0)
			{
				return 1;
			}
			else if(val == 0)
			{
				return 0;
			}
			else
			{
				return -1;
			}
		}
		else
		{
			return -1;
		}
	}

	/**
	 *
	 */
	public TimeInterval __sub__(Time tm)
	{
		return TimeFactory.getInstance().createTimeInterval(
				(int) (this.getTimeInMinutes() - tm.getTimeInMinutes()),
				TimeInterval.MIN_INTERVAL);
	}

	/**
	 *
	 */
	public Time __add__(TimeInterval ti)
	{
		Time tm = this.create(this);
		tm.incrementBy(ti);
		return tm;
	}

	/**
	 *
	 */
	public Time __radd__(TimeInterval ti)
	{
		return __add__(ti);
	}

	/**
	 *
	 */
	public Time __sub__(TimeInterval ti)
	{
		Time tm = this.create(this);
		TimeInterval ti2 = ti.createByMultiplying(-1);
		tm.incrementBy(ti2);
		return tm;
	}

	/**
	 *
	 */
	public Time __rsub__(TimeInterval ti)
	{
		return __sub__(ti);
	}

	/**
	 *
	 */
	public Time __add__(String tistr)
	{
		TimeInterval ti = TimeFactory.getTimeIntervalFromString(tistr);
		return __add__(ti);
	}

	/**
	 *
	 */
	public Time __radd__(String tistr)
	{
		TimeInterval ti = TimeFactory.getTimeIntervalFromString(tistr);
		return __add__(ti);
	}

	/**
	 *
	 */
	public Object __sub__(String tistr)
	{
		TimeInterval ti = null;
		Time tm = null;
		try
		{
			ti = TimeFactory.getTimeIntervalFromString(tistr);
		}
		catch(Exception e)
		{
		}
		if(ti == null)
		{
			try
			{
				tm = TimeFactory.getTimeFromString(tistr);
			}
			catch(Exception e)
			{
				throw Py.TypeError("time interval or time string expected");
			}
		}
		if(ti != null)
		{
			return __sub__(ti);
		}
		else
		{
			return __sub__(tm);
		}
	}

	/**
	 *
	 */
	public Object __rsub__(String tistr)
	{
		return __sub__(tistr);
	}

}

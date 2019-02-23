/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.time;

import java.io.Serializable;
import java.util.Calendar;
import java.util.Date;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.TimeZone;

/**
 * A class encapsulating the interval associated with a time series. This can be
 * represented by a string such as 1hour, 1min, 1hour_5min etcetra. This is
 * meaningful really in the context of a regular time series.
 * 
 * @author Nicky Sandhu
 * @version $Id: DefaultTimeInterval.java,v 1.1 2003/10/02 20:49:35 redwood Exp
 *          $
 */
public class DefaultTimeInterval implements TimeInterval, Serializable {
	/**
	 * The delimiter in the sequence of string containing more than one interval
	 * string. E.g. 1day_5hours_3mins.
	 */
	public static String DELIMITER = "_";
	/**
	 * an array of number of intervals indexed by interval type
	 */
	private int[] _numberOfIntervals = new int[9];

	/**
   *
   */
	DefaultTimeInterval() {
		for (int i = 0; i < _numberOfIntervals.length; i++)
			_numberOfIntervals[i] = 0;
	}

	/**
	 * create copy of self
	 * 
	 * @return copy of self
	 */
	public TimeInterval create(TimeInterval ti) {
		DefaultTimeInterval nti = new DefaultTimeInterval();
		for (int i = 0; i < _numberOfIntervals.length; i++)
			nti._numberOfIntervals[i] = this._numberOfIntervals[i];
		return nti;
	}

	/**
	 * creates a time interval from number of intervals and type of interval.
	 * 
	 * @param numberOfIntervals
	 *            The number of intervals
	 * @param intervalType
	 *            The type of interval as defined by XXX_INTERVAL constants in
	 *            this interface.
	 * 
	 */
	public TimeInterval create(int numberOfIntervals, int intervalType) {
		DefaultTimeInterval ti = new DefaultTimeInterval();
		ti._numberOfIntervals[intervalType] = numberOfIntervals;
		return ti;
	}

	/**
	 * @param intervalRep
	 *            A underscore delimited string of sign, integer and interval
	 *            type strings. E.g. 1day_+6hours or 3years_-5months_+3mins
	 *            create a interval in minutes from a string.
	 */
	public TimeInterval create(String intervalRep) {
		DefaultTimeInterval ti = new DefaultTimeInterval();
		try {
			StringTokenizer st = new StringTokenizer(intervalRep, DELIMITER);
			while (st.hasMoreTokens()) {
				String token = st.nextToken();
				parseIntoInterval(ti, token);
			}
			// if ( intervalRep.indexOf(DELIMITER) > 0 ) {
			// } else {
			// String token = new String(intervalRep);
			// parseIntoInterval(ti,token);
			// }
		} catch (NumberFormatException nfe) {
			throw new IllegalArgumentException("Time Interval: " + intervalRep
					+ " has invalid digit");
		} catch (NoSuchElementException nsee) {
			throw new IllegalArgumentException("Time Interval: " + intervalRep
					+ " has invalid string");
		}
		return ti;
	}

	/**
	 * parses token of the form <DIGIT><INTERVAL_STR> into the correct postion
	 * in the interval array
	 */
	private void parseIntoInterval(DefaultTimeInterval ti, String token) {
		token = token.toUpperCase();
		int interval = 0;
		if (token.indexOf(MIN_INTERVAL_STR) > 0) {
			interval = new Integer(token.substring(0, token
					.indexOf(MIN_INTERVAL_STR))).intValue();
			ti._numberOfIntervals[MIN_INTERVAL] += interval;
		} else if (token.indexOf(HOUR_INTERVAL_STR) > 0) {
			interval = new Integer(token.substring(0, token
					.indexOf(HOUR_INTERVAL_STR))).intValue();
			ti._numberOfIntervals[HOUR_INTERVAL] += interval;
		} else if (token.indexOf(DAY_INTERVAL_STR) > 0) {
			interval = new Integer(token.substring(0, token
					.indexOf(DAY_INTERVAL_STR))).intValue();
			ti._numberOfIntervals[DAY_INTERVAL] += interval;
		} else if (token.indexOf(WEEK_INTERVAL_STR) > 0) {
			interval = new Integer(token.substring(0, token
					.indexOf(WEEK_INTERVAL_STR))).intValue();
			ti._numberOfIntervals[WEEK_INTERVAL] += interval;
		} else if (token.indexOf(MONTH_INTERVAL_STR) > 0) {
			interval = new Integer(token.substring(0, token
					.indexOf(MONTH_INTERVAL_STR))).intValue();
			ti._numberOfIntervals[MONTH_INTERVAL] += interval;
		} else if (token.indexOf(YEAR_INTERVAL_STR) > 0) {
			interval = new Integer(token.substring(0, token
					.indexOf(YEAR_INTERVAL_STR))).intValue();
			ti._numberOfIntervals[YEAR_INTERVAL] += interval;
		} else if (token.indexOf(DECADE_INTERVAL_STR) > 0) {
			interval = new Integer(token.substring(0, token
					.indexOf(DECADE_INTERVAL_STR))).intValue();
			ti._numberOfIntervals[DECADE_INTERVAL] += interval;
		} else if (token.indexOf(CENTURY_INTERVAL_STR) > 0) {
			interval = new Integer(token.substring(0, token
					.indexOf(CENTURY_INTERVAL_STR))).intValue();
			ti._numberOfIntervals[CENTURY_INTERVAL] += interval;
		}
	}

	/**
	 * gets the number of intervals of type field. This field is one of the
	 * XXX_INTERVAL types.
	 * 
	 * @return the number of intervals of type field
	 */
	public int getNumberOfIntervals(int field) {
		int n = 0;
		if (field >= 1 && field <= 8)
			n = _numberOfIntervals[field];
		return n;
	}

	/**
	 * true if current time matters in converting interval to minutes
	 */
	public boolean isTimeContextDependent() {
		return (_numberOfIntervals[MONTH_INTERVAL] != 0
				|| _numberOfIntervals[YEAR_INTERVAL] != 0
				|| _numberOfIntervals[DECADE_INTERVAL] != 0 || _numberOfIntervals[CENTURY_INTERVAL] != 0);
	}

	/**
	 * converts current interval to minutes depending upon current time.
	 */
	public long getIntervalInMinutes(Time time) {
		Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		Date d = time == null ? new Date() : time.getDate();
		c.setTime(d);
		for(int i=0; i < _numberOfIntervals.length; i++){
			int calendarField = Calendar.MINUTE;
			int amount = _numberOfIntervals[i];
			switch (i) {
			case MIN_INTERVAL:
				calendarField=Calendar.MINUTE;
				break;
			case HOUR_INTERVAL:
				calendarField=Calendar.HOUR;
				break;
			case DAY_INTERVAL:
				calendarField=Calendar.DATE;
				break;
			case WEEK_INTERVAL:
				calendarField=Calendar.WEEK_OF_MONTH;
				break;
			case MONTH_INTERVAL:
				calendarField=Calendar.MONTH;
				break;
			case YEAR_INTERVAL:
				calendarField=Calendar.YEAR;
				break;
			case DECADE_INTERVAL:
				calendarField=Calendar.YEAR;
				amount=amount*10;
				break;
			case CENTURY_INTERVAL:
				calendarField=Calendar.YEAR;
				amount=amount*100;
				break;
			default:
				calendarField=Calendar.MINUTE;
			}
			c.add(calendarField, amount);
		}
		long millis = c.getTime().getTime()-d.getTime();
		return millis/60000;
	}

	/**
	 * @return the string associated with type of interval
	 */
	public String getIntervalAsString() {
		StringBuffer buf = new StringBuffer(200);
		int index = 0;
		for (int i = 1; i < _numberOfIntervals.length; i++) {
			if (_numberOfIntervals[i] != 0) {
				if (index != 0)
					buf.append(DELIMITER);
				index++;
				buf.append(_numberOfIntervals[i]).append(getIntervalString(i));
			}
		}
		return buf.toString();
	}

	/**
   *
   */
	public String getFieldName(int field) {
		return getIntervalString(field);
	}

	/**
   *
   */
	public static String getIntervalString(int id) {
		switch (id) {
		case MIN_INTERVAL:
			return MIN_INTERVAL_STR;
		case HOUR_INTERVAL:
			return HOUR_INTERVAL_STR;
		case DAY_INTERVAL:
			return DAY_INTERVAL_STR;
		case WEEK_INTERVAL:
			return WEEK_INTERVAL_STR;
		case MONTH_INTERVAL:
			return MONTH_INTERVAL_STR;
		case YEAR_INTERVAL:
			return YEAR_INTERVAL_STR;
		case DECADE_INTERVAL:
			return DECADE_INTERVAL_STR;
		case CENTURY_INTERVAL:
			return CENTURY_INTERVAL_STR;
		default:
			return "";
		}
	}

	/**
	 * Construct a time interval representing a time interval as calculated
	 * using the given time interval and then converting that interval to
	 * minutes as needed using this factor
	 * 
	 * @return a new time interval
	 */
	public TimeInterval createByMultiplying(int factor) {
		DefaultTimeInterval nti = new DefaultTimeInterval();
		for (int i = 1; i < _numberOfIntervals.length; i++) {
			nti._numberOfIntervals[i] = factor * this._numberOfIntervals[i];
		}
		return nti;
	}

	/**
	 * Construct a time interval representing a time interval as calculated
	 * using the given time interval and then converting that interval to
	 * minutes as needed using this scalar
	 * 
	 * @return a new time interval
	 */
	public TimeInterval createByAdding(TimeInterval ti) {
		DefaultTimeInterval nti = new DefaultTimeInterval();
		for (int i = 1; i < _numberOfIntervals.length; i++) {
			nti._numberOfIntervals[i] = this._numberOfIntervals[i]
					+ ti.getNumberOfIntervals(i);
		}
		return nti;
	}

	/**
	 * returns 0 if equal, -ve if less than this interval and +ve if more than
	 * this interval
	 */
	public int compare(TimeInterval ti) {
		long diff = 0;
		for (int field = 8; field >= 1; field--) {
			diff += (getNumberOfIntervals(field) - ti
					.getNumberOfIntervals(field))
					* getMaximumNumberOfMinutesInField(field);
		}
		if (diff == 0)
			return 0;
		else if (diff > 0)
			return 1;
		else
			return -1;
	}

	/**
   *
   */
	static int getMaximumNumberOfMinutesInField(int field) {
		switch (field) {
		case MIN_INTERVAL:
			return 1;
		case HOUR_INTERVAL:
			return 60;
		case DAY_INTERVAL:
			return 1440;
		case WEEK_INTERVAL:
			return 10080;
		case MONTH_INTERVAL:
			return 44640; // 31 day month
		case YEAR_INTERVAL:
			return 527040; // leap year
		case DECADE_INTERVAL:
			return 5260320; // 3 leap years max
		case CENTURY_INTERVAL:
			return 52598880; // 27 leap years max
		default:
			return 0;
		}
	}

	/**
   *
   */
	static int getMinimumNumberOfMinutesInField(int field) {
		switch (field) {
		case MIN_INTERVAL:
			return 1;
		case HOUR_INTERVAL:
			return 60;
		case DAY_INTERVAL:
			return 1440;
		case WEEK_INTERVAL:
			return 10080;
		case MONTH_INTERVAL:
			return 40320; // 28 day month
		case YEAR_INTERVAL:
			return 525600; // no leap year
		case DECADE_INTERVAL:
			return 5257440; // 1 leap years min
		case CENTURY_INTERVAL:
			return 52588800; // 20 leap years max
		default:
			return 0;
		}
	}

	/**
	 * returns string representation of interval
	 */
	public String toString() {
		return getIntervalAsString();
	}

	/**
   *
   */
	public TimeInterval __add__(TimeInterval ti) {
		return this.createByAdding(ti);
	}

	/**
   *
   */
	public TimeInterval __sub__(TimeInterval ti) {
		TimeInterval ti2 = ti.createByMultiplying(-1);
		return createByAdding(ti2);
	}

	/**
   *
   */
	public TimeInterval __mul__(int factor) {
		return createByMultiplying(factor);
	}

	/**
   *
   */
	public TimeInterval __rmul__(int factor) {
		return __mul__(factor);
	}

	/**
   *
   */
	public int __div__(TimeInterval ti) {
		Time tm = TimeFactory.getInstance().createTime(0);
		tm.incrementBy(this);
		Time tm2 = TimeFactory.getInstance().createTime(0);
		long ncount = 0;
		try {
			ncount = tm2.getNumberOfIntervalsTo(tm, ti);
		} catch (RuntimeException re) {
			System.out.println("Inexact division");
		}
		return (int) ncount;
	}

	/**
	 * for python
	 */
	public TimeInterval __add__(String tistr) {
		TimeInterval ti = TimeFactory.getTimeIntervalFromString(tistr);
		return __add__(ti);
	}

	/**
	 * for python
	 */
	public TimeInterval __radd__(String tistr) {
		return __add__(tistr);
	}

	/**
	 * for python
	 */
	public TimeInterval __sub__(String tistr) {
		TimeInterval ti = TimeFactory.getTimeIntervalFromString(tistr);
		return __sub__(ti);
	}

	/**
	 * for python
	 */
	public TimeInterval __rsub__(String tistr) {
		return __sub__(tistr);
	}

	/**
	 * for python
	 */
	public int __div__(String tistr) {
		TimeInterval ti = TimeFactory.getTimeIntervalFromString(tistr);
		return __div__(ti);
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.time;

/**
 * A interface encapsulating the interval associated with a time series.
 * <p>
 * Intervals can be converted to a time interval in minutes, however some
 * intervals are independent of the current time context such as minutes, hours,
 * days and other intervals have to be interpreted in relation to the current
 * time e.g. months, years, decades, centuries.
 * <p>
 * This is meaningful really in the context of a regular time series.
 * FIXME: Time interval should have a conversion method to change say 60 min interval to 1 hour interval
 *
 * @author Nicky Sandhu
 * @version $Id: TimeInterval.java,v 1.1 2003/10/02 20:49:37 redwood Exp $
 */
public interface TimeInterval extends java.io.Serializable
{
	/**
	 * minute interval. The smallest interval represented by this interface
	 */
	int MIN_INTERVAL = 1;
	/**
	 * hour interval = 60 minutes
	 */
	int HOUR_INTERVAL = 2;
	/**
	 * day interval = 24 hours
	 */
	int DAY_INTERVAL = 3;
	/**
	 * week interval = 7 days
	 */
	int WEEK_INTERVAL = 4;
	/**
	 * month interval ~ (28,29,30,31) days
	 */
	int MONTH_INTERVAL = 5;
	/**
	 * year interval = 12 months
	 */
	int YEAR_INTERVAL = 6;
	/**
	 * decade interval = 10 years
	 */
	int DECADE_INTERVAL = 7;
	/**
	 * century interval = 100 years
	 */
	int CENTURY_INTERVAL = 8;
	/**
	 *
	 */
	String MIN_INTERVAL_STR = "MIN";
	/**
	 *
	 */
	String HOUR_INTERVAL_STR = "HOUR";
	/**
	 *
	 */
	String DAY_INTERVAL_STR = "DAY";
	/**
	 *
	 */
	String WEEK_INTERVAL_STR = "WEEK";
	/**
	 *
	 */
	String MONTH_INTERVAL_STR = "MON";
	/**
	 *
	 */
	String YEAR_INTERVAL_STR = "YEAR";
	/**
	 *
	 */
	String DECADE_INTERVAL_STR = "DECADE";
	/**
	 *
	 */
	String CENTURY_INTERVAL_STR = "CENTURY";

	/**
	 * create copy of self
	 *
	 * @return copy of self
	 */
	TimeInterval create(TimeInterval ti);

	/**
	 * creates a time interval from number of intervals and type of interval.
	 *
	 * @param numberOfIntervals The number of intervals
	 * @param intervalType      The type of interval as defined by XXX_INTERVAL constants in
	 *                          this interface.
	 */
	TimeInterval create(int numberOfIntervals, int intervalType);

	/**
	 * @param intervalRep A underscore delimited string of sign, integer and interval
	 *                    type strings. E.g. 1day_+6hours or 3years_-5months_+3mins
	 *                    create a interval in minutes from a string.
	 */
	TimeInterval create(String intervalRep);

	/**
	 * gets the number of intervals of type field. This field is one of the
	 * XXX_INTERVAL types.
	 *
	 * @return the number of intervals of type field
	 */
	int getNumberOfIntervals(int field);

	/**
	 * @return a string representing the field
	 */
	String getFieldName(int field);

	/**
	 * true if current time matters in converting interval to minutes
	 */
	boolean isTimeContextDependent();

	/**
	 * converts current interval to minutes depending upon current time.
	 */
	long getIntervalInMinutes(Time time);

	/**
	 * @return the string associated with type of interval
	 */
	String getIntervalAsString();

	/**
	 * Construct a time interval representing a time interval as calculated
	 * using this interval and the multiplying factor.
	 *
	 * @return a new time interval
	 */
	TimeInterval createByMultiplying(int factor);

	/**
	 * Construct a time interval representing a time interval as calculated
	 * using this time interval and adding it to the given time interval
	 *
	 * @return a new time interval
	 */
	TimeInterval createByAdding(TimeInterval ti);

	/**
	 * returns 0 if equal, -ve if less than this interval and +ve if more than
	 * this interval
	 */
	int compare(TimeInterval ti);

	/**
	 * for python
	 */
	TimeInterval __add__(TimeInterval ti);

	/**
	 * for python
	 */
	TimeInterval __sub__(TimeInterval ti);

	/**
	 * for python
	 */
	TimeInterval __mul__(int factor);

	/**
	 * for python
	 */
	TimeInterval __rmul__(int factor);

	/**
	 * for python
	 */
	int __div__(TimeInterval ti);

	/**
	 * for python
	 */
	TimeInterval __add__(String tistr);

	/**
	 * for python
	 */
	TimeInterval __sub__(String tistr);

	/**
	 * for python
	 */
	int __div__(String tistr);
}

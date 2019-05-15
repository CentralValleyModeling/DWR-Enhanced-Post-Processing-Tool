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

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Formats date according to HEC DSS criteria. namely midnight is interpreted as
 * 2400 of the previous day.
 * 
 * @author Nicky Sandhu
 * @version $Id: DefaultTimeFormat.java,v 1.1 2003/10/02 20:49:35 redwood Exp $
 */
public class DefaultTimeFormat extends TimeFormat {
	/**
	 * creates a time format for the given pattern
	 */
	public TimeFormat create(String pattern) {
		return new DefaultTimeFormat(pattern);
	}

	/**
	 * Use pattern for US locale
	 */
	public DefaultTimeFormat() {
		this("ddMMMyyyy HHmm", Locale.US);
	}

	/**
	 * Use pattern for US locale
	 */
	public DefaultTimeFormat(String pattern) {
		this(pattern, Locale.US);
	}

	/**
	 * sets up formatting by given string and given locale. Time zone is assumed
	 * to be GMT. This does not have the daylight savings time adjustments.
	 */
	public DefaultTimeFormat(String pattern, Locale locale) {
		_pattern = pattern;
		_formatter = new SimpleDateFormat(pattern, locale);
		_formatter.setTimeZone(TimeZone.getTimeZone("GMT"));
	}

	/**
	 * Formats a DefaultTime into a date/time string. The only difference with
	 * java format is the conversion of 0000 hour string to 2400 and pushing
	 * back the data by one day. Thus 01JAN1900 0000 becomes 31DEC1899 2400.
	 * Rest of the times are as returned by the format of java.text.DateFormat
	 * 
	 * @return the formatted date/time string.
	 */
	public StringBuffer format(Date date, StringBuffer toAppendTo,
			FieldPosition fieldPosition) {
		String str = _formatter.format(date, toAppendTo, fieldPosition)
				.toString();
		if (_pattern.indexOf("HH") > 0) {
			if (str.indexOf("0000") > 0) {
				Date d = new Date(date.getTime() - 60000);
				String str2 = _formatter.format(d);
				int pos = str2.indexOf("2359");
				if (pos != -1) {
					str = str2.substring(0, pos);
					str = str.concat("2400");
				}
			}
		}

		return new StringBuffer(str);
	}

	/**
	 * Parse a date/time string according to the given parse position. For
	 * example, a time text "07/10/96 4:5 PM, PDT" will be parsed into a Date
	 * that is equivalent to Date(837039928046).
	 * 
	 */
	public Date parse(String text, ParsePosition pos) {
		Date date = _formatter.parse(text, pos);
		return date;
	}

	/**
    *
    */
	public String toString() {
		return "Default Time Format: " + _pattern;
	}

	/**
	 * The format object
	 */
	private DateFormat _formatter;
	/**
	 * the format pattern
	 */
	private String _pattern;
}

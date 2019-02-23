/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.time;

import java.text.FieldPosition;
import java.text.ParsePosition;
import java.util.Date;
import java.util.Locale;

/**
 * Formats date according to HEC DSS criteria. namely midnight is interpreted as
 * 2400 of the previous day. It accepts patterns based on substrings of
 * ddMMMyyyy HHmm only. It then parses the correct string after using
 * DefaultTimeFormat to first format the time into a string.
 * 
 * @author Nicky Sandhu
 * @version $Id: SubTimeFormat.java,v 1.1 2003/10/02 20:49:36 redwood Exp $
 */
public class SubTimeFormat extends TimeFormat {
	/**
	 * creates a time format for the given pattern
	 */
	public TimeFormat create(String pattern) {
		return new SubTimeFormat(pattern);
	}

	/**
	 * Use pattern for US locale
	 */
	public SubTimeFormat() {
		this("ddMMMyyyy HHmm", Locale.US);
	}

	/**
	 * Use pattern for US locale
	 */
	public SubTimeFormat(String pattern) {
		this(pattern, Locale.US);
	}

	/**
	 * sets up formatting by given string and given locale. Time zone is assumed
	 * to be GMT. This does not have the daylight savings time adjustments.
	 */
	public SubTimeFormat(String pattern, Locale locale) {
		_fullPattern = "ddMMMyyyy HHmm";
		_formatter = new DefaultTimeFormat(_fullPattern, locale);
		_pattern = pattern;
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
		StringBuffer fsb = new StringBuffer(_pattern);
		// for(int i=0; i < fsb.length(); i++) fsb.setCharAt(i,' ');
		String[] subPatternArray = { "dd", "MMM", "yyyy", "HH", "mm" };
		for (int k = 0; k < subPatternArray.length; k++) {
			String pstr = subPatternArray[k];
			int i = _pattern.indexOf(pstr);
			int j = _fullPattern.indexOf(pstr);
			if (i < 0 || j < 0)
				continue;
			int lp = pstr.length();
			for (int m = j; m < j + lp; m++) {
				fsb.setCharAt(i + m - j, str.charAt(m));
			}
		}
		return fsb;
		// return new StringBuffer(fsb.toString().trim());
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
		return "SubTimeFormat: " + _pattern;
	}

	/**
	 * The format object
	 */
	private DefaultTimeFormat _formatter;
	/**
	 * the format pattern
	 */
	private String _pattern, _fullPattern;
}

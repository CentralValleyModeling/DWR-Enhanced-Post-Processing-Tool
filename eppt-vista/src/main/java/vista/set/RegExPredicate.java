/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import java.util.regex.Pattern;

/**
 * Uses a regular expression to determine filtering.
 * @param <T>
 */
public abstract class RegExPredicate<T> implements Predicate<T> {

	protected Pattern _pattern;

	/**
	 * initializes the regular expression compilers
	 */
	public RegExPredicate(String regex) {
		setRegularExpression(regex);
	}

	/**
	 * sets the regular expression. This may throw a runtime exception if the
	 * grammar and regular expression do not match.
	 */
	public final void setRegularExpression(String regex) {
		_regularExp = regex.toUpperCase();
		initRegEx(_regularExp);
	}

	/**
	 * returns the regular expression in use.
	 */
	public final String getRegularExpression() {
		return _regularExp;
	}

	/**
	 * initializes the regular expression by compiling it into the grammar
	 */
	private void initRegEx(String regex) {
		_pattern = Pattern.compile(regex);
	}

	/**
	 * regular expression to be matched
	 */
	private String _regularExp;
}

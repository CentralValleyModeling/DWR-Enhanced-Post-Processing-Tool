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

import java.util.regex.Pattern;

/**
 * Uses a regular expression to determine filtering.
 *
 * @param <T>
 */
public abstract class RegExPredicate<T> implements Predicate<T>
{

	protected Pattern _pattern;
	/**
	 * regular expression to be matched
	 */
	private String _regularExp;

	/**
	 * initializes the regular expression compilers
	 */
	public RegExPredicate(String regex)
	{
		setRegularExpression(regex);
	}

	/**
	 * returns the regular expression in use.
	 */
	public final String getRegularExpression()
	{
		return _regularExp;
	}

	/**
	 * sets the regular expression. This may throw a runtime exception if the
	 * grammar and regular expression do not match.
	 */
	public final void setRegularExpression(String regex)
	{
		_regularExp = regex.toUpperCase();
		initRegEx(_regularExp);
	}

	/**
	 * initializes the regular expression by compiling it into the grammar
	 */
	private void initRegEx(String regex)
	{
		_pattern = Pattern.compile(regex);
	}
}

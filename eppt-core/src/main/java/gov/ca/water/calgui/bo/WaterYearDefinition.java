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

package gov.ca.water.calgui.bo;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.Locale;
import java.util.Optional;

import hec.lang.annotation.Scriptable;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-20-2019
 */
public class WaterYearDefinition
{
	private final String _name;
	private final Month _start;
	private final Month _end;
	private final Integer _startDefaultYear;
	private final Integer _endDefaultYear;

	@Scriptable
	public WaterYearDefinition(String name, Month start, Month end)
	{
		this(name, start, end, null, null);
	}

	public WaterYearDefinition(String name, Month start, Month end, Integer startDefaultYear, Integer endDefaultYear)
	{
		_name = name;
		_start = start;
		_end = end;
		_startDefaultYear = startDefaultYear;
		_endDefaultYear = endDefaultYear;
	}

	public String getName()
	{
		return _name;
	}

	public Month getStartMonth()
	{
		return _start;
	}

	public Month getEndMonth()
	{
		return _end;
	}

	@Override
	public String toString()
	{
		return String.format("%s (%s - %s)", _name, _start.getDisplayName(TextStyle.SHORT, Locale.getDefault()),
				_end.getDisplayName(TextStyle.SHORT, Locale.getDefault()));
	}

	public Optional<Integer> getEndDefaultYear()
	{
		return Optional.ofNullable(_endDefaultYear);
	}

	public Optional<Integer> getStartDefaultYear()
	{
		return Optional.ofNullable(_startDefaultYear);
	}
}

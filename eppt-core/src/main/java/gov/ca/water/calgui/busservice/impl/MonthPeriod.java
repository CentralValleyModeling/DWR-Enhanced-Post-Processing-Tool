/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.busservice.impl;

import java.time.Month;
import java.time.YearMonth;
import java.time.format.TextStyle;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-06-2020
 */
public class MonthPeriod
{
	private final Month _start;
	private final Month _end;
	private final String _name;

	public MonthPeriod(String name, Month start, Month end)
	{
		_name = name;
		_start = start;
		_end = end;
	}

	public Month getStart()
	{
		return _start;
	}

	public Month getEnd()
	{
		return _end;
	}

	@Override
	public String toString()
	{
		return getName();
	}

	private String formatMonth(Month month)
	{
		if(month != null)
		{
			return month.getDisplayName(TextStyle.SHORT, Locale.US);
		}
		else
		{
			return "";
		}
	}

	public List<YearMonth> getYearMonths(int year)
	{
		List<YearMonth> retval = new ArrayList<>();
		if(getStart() == null || getEnd() == null)
		{
			return retval;
		}
		if(getStart() != getEnd())
		{
			int i = 0;
			long between = ChronoUnit.MONTHS.between(YearMonth.of(2001, getStart()), YearMonth.of(2010, getEnd())) % 12;
			Month month = getStart().plus(i);
			YearMonth yearMonth;
			if(isPreviousYear(month))
			{
				yearMonth = YearMonth.of(year - 1, month);
			}
			else
			{
				yearMonth = YearMonth.of(year, month);
			}
			while(retval.size() < between + 1)
			{
				retval.add(yearMonth.plusMonths(i));
				i++;
			}
		}
		else
		{
			if(isPreviousYear(getStart()))
			{
				retval.add(YearMonth.of(year - 1, getStart()));
			}
			else
			{
				retval.add(YearMonth.of(year, getStart()));
			}
		}
		return retval;
	}

	private boolean isPreviousYear(Month month)
	{
		long totalMonths = 1 + ChronoUnit.MONTHS.between(YearMonth.of(2001, getStart()), YearMonth.of(2010, getEnd())) % 12;
		if(totalMonths == 12 && getStart() == Month.JANUARY)
		{
			return false;
		}
		return month == Month.OCTOBER || month == Month.NOVEMBER || month == Month.DECEMBER;
	}

	public String getName()
	{
		if(_name != null && !_name.isEmpty())
		{
			return _name;
		}
		else if(Objects.equals(getStart(), getEnd()))
		{
			return formatMonth(getStart());
		}
		else
		{
			return formatMonth(getStart()) + " - " + formatMonth(getEnd());
		}
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
		{
			return true;
		}
		if(o == null || getClass() != o.getClass())
		{
			return false;
		}
		final MonthPeriod that = (MonthPeriod) o;
		return getStart() == that.getStart() && getEnd() == that.getEnd() && getName().equals(that.getName());
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(getStart(), getEnd(), getName());
	}

	public int getWaterYear(YearMonth yearMonth)
	{
		int retval = yearMonth.getYear();
		if(!getYearMonths(retval).contains(yearMonth))
		{
			retval = retval - 1;
			if(!getYearMonths(retval).contains(yearMonth))
			{
				retval = retval + 2;
			}
		}
		return retval;
	}
}

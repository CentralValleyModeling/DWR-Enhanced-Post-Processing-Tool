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

	public MonthPeriod(Month start, Month end)
	{
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
		if(Objects.equals(_start, _end))
		{
			return formatMonth(_start);
		}
		else
		{
			return formatMonth(_start) + " - " + formatMonth(_end);
		}
	}

	private String formatMonth(Month month)
	{
		if(month != null)
		{
			return month.getDisplayName(TextStyle.FULL, Locale.US);
		}
		else
		{
			return "";
		}
	}

	public List<YearMonth> getYearMonths(int year)
	{
		List<YearMonth> retval = new ArrayList<>();
		if(_start == null || _end == null)
		{
			return retval;
		}
		if(_start != _end)
		{
			int i = 0;
			long between = ChronoUnit.MONTHS.between(YearMonth.of(2001, _start), YearMonth.of(2010, _end)) % 12;
			Month month = _start.plus(i);
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
			if(isPreviousYear(_start))
			{
				retval.add(YearMonth.of(year - 1, _start));
			}
			else
			{
				retval.add(YearMonth.of(year, _start));
			}
		}
		return retval;
	}

	private boolean isPreviousYear(Month month)
	{
		long totalMonths = 1 + ChronoUnit.MONTHS.between(YearMonth.of(2001, _start), YearMonth.of(2010, _end)) % 12;
		if(totalMonths == 12 && _start == Month.JANUARY)
		{
			return false;
		}
		return month == Month.OCTOBER || month == Month.NOVEMBER || month == Month.DECEMBER;
	}

}

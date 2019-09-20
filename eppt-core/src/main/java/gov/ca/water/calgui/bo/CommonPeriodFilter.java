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

import java.time.LocalDateTime;
import java.util.Map;
import java.util.TreeMap;

import static java.util.stream.Collectors.toMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-06-2019
 */
public class CommonPeriodFilter implements PeriodFilter
{
	private final LocalDateTime _start;
	private final LocalDateTime _end;

	public CommonPeriodFilter(LocalDateTime start, LocalDateTime end)
	{
		_start = start;
		_end = end;
	}

	@Override
	public boolean test(Map.Entry<LocalDateTime, Double> e)
	{
		LocalDateTime key = e.getKey();
		return (key.isEqual(_start) || key.isAfter(_start)) &&
				(key.isEqual(_end) || key.isBefore(_end));
	}

	public LocalDateTime getStart()
	{
		return _start;
	}

	public LocalDateTime getEnd()
	{
		return _end;
	}
}

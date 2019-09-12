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
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static java.util.stream.Collectors.toMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-27-2019
 */
public class WaterYearPeriodRangesFilter implements PeriodFilter
{
	private final List<WaterYearPeriodRange> _waterYearPeriodRanges;
	private final WaterYearDefinition _waterYearDefinition;

	public WaterYearPeriodRangesFilter(List<WaterYearPeriodRange> waterYearPeriodRanges, WaterYearDefinition waterYearDefinition)
	{
		_waterYearPeriodRanges = waterYearPeriodRanges;
		_waterYearDefinition = waterYearDefinition;
	}

	@Override
	public boolean test(Map.Entry<LocalDateTime, Double> input)
	{
		boolean retval = false;
		for(WaterYearPeriodRange range : _waterYearPeriodRanges)
		{

			YearMonth start = range.getStart(_waterYearDefinition);
			YearMonth end = range.getEnd(_waterYearDefinition);
			LocalDateTime key = input.getKey();
			YearMonth inputYearMonth = YearMonth.of(key.getYear(), key.getMonth());
			retval = inputYearMonth.equals(start) || inputYearMonth.equals(end)
					|| (start.isBefore(inputYearMonth) && end.isAfter(inputYearMonth));
			if(retval)
			{
				break;
			}
		}
		return retval;
	}
}

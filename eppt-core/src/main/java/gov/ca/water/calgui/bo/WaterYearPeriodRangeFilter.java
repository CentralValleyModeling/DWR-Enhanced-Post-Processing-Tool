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
import java.util.Map;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-27-2019
 */
public class WaterYearPeriodRangeFilter implements PeriodFilter
{
	private final WaterYearPeriodRange _waterYearPeriodRange;
	private final WaterYearDefinition _waterYearDefinition;

	public WaterYearPeriodRangeFilter(WaterYearPeriodRange waterYearPeriodRange, WaterYearDefinition waterYearDefinition)
	{
		_waterYearPeriodRange = waterYearPeriodRange;
		_waterYearDefinition = waterYearDefinition;
	}

	@Override
	public boolean test(Map.Entry<LocalDateTime, Double> input)
	{
		//Shifting the ranges because values are EOP
		LocalDateTime key = input.getKey();
		YearMonth inputYearMonth = YearMonth.of(key.getYear(), key.getMonth()).minusMonths(1);

		return testYearMonth(inputYearMonth);
	}

	public boolean testYearMonth(YearMonth inputYearMonth)
	{
		YearMonth start = _waterYearPeriodRange.getStart(_waterYearDefinition);
		YearMonth end = _waterYearPeriodRange.getEnd(_waterYearDefinition);
		return inputYearMonth.equals(start) || inputYearMonth.equals(end)
				 || (start.isBefore(inputYearMonth) && end.isAfter(inputYearMonth));
	}

	public WaterYearPeriodRange getWaterYearPeriodRange()
	{
		return _waterYearPeriodRange;
	}
}

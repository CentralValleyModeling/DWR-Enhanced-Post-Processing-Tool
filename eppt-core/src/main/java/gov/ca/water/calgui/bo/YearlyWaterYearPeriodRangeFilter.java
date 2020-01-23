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
public class YearlyWaterYearPeriodRangeFilter implements AnnualPeriodFilter
{
	private final WaterYearPeriodRange _waterYearPeriodRange;

	public YearlyWaterYearPeriodRangeFilter(WaterYearPeriodRange waterYearPeriodRange)
	{
		_waterYearPeriodRange = waterYearPeriodRange;
	}

	@Override
	public boolean test(Map.Entry<Integer, Double> input)
	{
		int startYear = _waterYearPeriodRange.getStartYear().getYear();
		int endYear = _waterYearPeriodRange.getEndYear().getYear();
		return input.getKey() >= startYear && input.getKey() <= endYear;
	}

	public WaterYearPeriodRange getWaterYearPeriodRange()
	{
		return _waterYearPeriodRange;
	}
}

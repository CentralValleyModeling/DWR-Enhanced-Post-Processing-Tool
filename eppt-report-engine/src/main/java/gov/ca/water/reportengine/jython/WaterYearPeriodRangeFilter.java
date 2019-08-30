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

package gov.ca.water.reportengine.jython;

import java.time.LocalDateTime;
import java.time.YearMonth;
import java.util.Map;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;

import static java.util.stream.Collectors.toMap;

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
	public Map<LocalDateTime, Double> filter(Map<LocalDateTime, Double> input)
	{
		YearMonth start = _waterYearPeriodRange.getStart(_waterYearDefinition);
		YearMonth end = _waterYearPeriodRange.getEnd(_waterYearDefinition);
		return input.entrySet().stream()
					.filter(e -> e.getKey().getYear() >= start.getYear())
					.filter(e -> e.getKey().getMonthValue() >= start.getMonthValue())
					.filter(e -> e.getKey().getYear() <= end.getYear())
					.filter(e -> e.getKey().getMonthValue() <= end.getMonthValue())
					.collect(toMap(Map.Entry::getKey, Map.Entry::getValue));
	}
}

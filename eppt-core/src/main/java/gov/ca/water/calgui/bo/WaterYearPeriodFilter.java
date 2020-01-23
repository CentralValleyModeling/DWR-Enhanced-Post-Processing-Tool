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
import java.time.Month;
import java.time.YearMonth;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;

import gov.ca.water.calgui.bo.PeriodFilter;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearType;

import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-27-2019
 */
public class WaterYearPeriodFilter implements PeriodFilter
{
	private final WaterYearPeriod _waterYearPeriod;
	private final WaterYearIndex _waterYearIndex;
	private final WaterYearDefinition _waterYearDefinition;

	public WaterYearPeriodFilter(WaterYearPeriod waterYearPeriod, WaterYearIndex waterYearIndex, WaterYearDefinition waterYearDefinition)
	{
		_waterYearPeriod = waterYearPeriod;
		_waterYearIndex = waterYearIndex;
		_waterYearDefinition = waterYearDefinition;
	}

	@Override
	public boolean test(Map.Entry<LocalDateTime, Double> input)
	{
		//Shifting the ranges because values are EOP
		LocalDateTime key = input.getKey().minusMonths(1);
		YearMonth keyYearMonth = YearMonth.of(key.getYear(), key.getMonth());
		Predicate<WaterYearPeriodRange> equals = range -> range.getStart(_waterYearDefinition).equals(keyYearMonth)
				|| range.getEnd(_waterYearDefinition).equals(keyYearMonth);
		Predicate<WaterYearPeriodRange> between = range -> range.getStart(_waterYearDefinition).isBefore(keyYearMonth)
				&& range.getEnd(_waterYearDefinition).isAfter(keyYearMonth);
		boolean retval =  _waterYearIndex.getWaterYearTypes().stream()
					   .filter(f -> f.getWaterYearPeriod().equals(_waterYearPeriod))
					   .map(WaterYearType::getYear)
					   .map(year -> new WaterYearPeriodRange(_waterYearPeriod,
							   new WaterYearType(year, _waterYearPeriod),
							   new WaterYearType(year, _waterYearPeriod)))
					   .anyMatch(equals.or(between));
		return retval;
	}

	public WaterYearIndex getWaterYearIndex()
	{
		return _waterYearIndex;
	}

	public WaterYearPeriod getWaterYearPeriod()
	{
		return _waterYearPeriod;
	}
}

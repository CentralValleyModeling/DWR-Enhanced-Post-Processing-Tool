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
import java.util.Map;
import java.util.Set;

import static java.util.stream.Collectors.toSet;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-27-2019
 */
public class YearlyWaterYearPeriodFilter implements AnnualPeriodFilter
{
	private final WaterYearPeriod _waterYearPeriod;
	private final WaterYearIndex _waterYearIndex;

	public YearlyWaterYearPeriodFilter(WaterYearPeriod waterYearPeriod, WaterYearIndex waterYearIndex)
	{
		_waterYearPeriod = waterYearPeriod;
		_waterYearIndex = waterYearIndex;
	}
	@Override
	public boolean test(Map.Entry<Integer, Double> input)
	{
		Set<Integer> years = _waterYearIndex.getWaterYearTypes().stream()
											.filter(f -> f.getWaterYearPeriod().equals(_waterYearPeriod))
											.map(WaterYearType::getYear)
											.collect(toSet());
		return years.contains(input.getKey());
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

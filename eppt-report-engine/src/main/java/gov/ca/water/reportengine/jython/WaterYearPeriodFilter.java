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
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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

	public WaterYearPeriodFilter(WaterYearPeriod waterYearPeriod, WaterYearIndex waterYearIndex)
	{
		_waterYearPeriod = waterYearPeriod;
		_waterYearIndex = waterYearIndex;
	}

	@Override
	public Map<LocalDateTime, Double> filter(Map<LocalDateTime, Double> input)
	{
		Set<Integer> years = _waterYearIndex.getWaterYearTypes().stream()
											  .filter(f -> f.getWaterYearPeriod().equals(_waterYearPeriod))
											  .map(WaterYearType::getYear)
											  .collect(toSet());
		return input.entrySet().stream()
					.filter(e->years.contains(e.getKey().getYear()))
					.collect(toMap(Map.Entry::getKey, Map.Entry::getValue));
	}
}

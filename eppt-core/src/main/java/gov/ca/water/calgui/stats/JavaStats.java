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

package gov.ca.water.calgui.stats;

import java.time.Month;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Objects;
import java.util.OptionalDouble;
import java.util.SortedMap;
import java.util.TreeMap;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.EpptReportingMonths;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-28-2020
 */
public abstract class JavaStats implements EpptStatistic
{
	@Override
	public SortedMap<Month, Double> calculateMonthly(SortedMap<Month, NavigableMap<Integer, Double>> data, WaterYearDefinition waterYearDefinition,
													 WaterYearIndex waterYearIndex, List<WaterYearIndex> waterYearIndices,
													 MonthPeriod monthPeriod)
	{
		Map<Month, Double> retval = new EnumMap<>(Month.class);
		for(Map.Entry<Month, NavigableMap<Integer, Double>> entry : data.entrySet())
		{
			NavigableMap<Integer, Double> values = entry.getValue();
			Month month = entry.getKey();
			OptionalDouble average = values.values().stream()
										   .filter(Objects::nonNull)
										   .filter(v -> !v.isNaN())
										   .mapToDouble(Double::doubleValue)
										   .average();
			if(average.isPresent())
			{
				retval.put(month, average.getAsDouble());
			}
			else
			{
				retval.put(month, null);
			}
		}
		return sort(retval, monthPeriod);
	}

	private SortedMap<Month, Double> sort(Map<Month, Double> calculate, MonthPeriod monthPeriod)
	{
		List<Month> months = EpptReportingMonths.getMonths(monthPeriod);
		SortedMap<Month, Double> retval = new TreeMap<>(Comparator.comparingInt(months::indexOf));
		retval.putAll(calculate);
		return retval;
	}

	@Override
	public Double calculateYearly(SortedMap<Integer, Double> data,
								  WaterYearDefinition waterYearDefinition,
								  WaterYearIndex waterYearIndex, List<WaterYearIndex> waterYearIndices)
	{
		Double retval = null;
		OptionalDouble average = data.values().stream().filter(Objects::nonNull).filter(v -> !v.isNaN()).mapToDouble(Double::doubleValue).average();
		if(average.isPresent())
		{
			retval = average.getAsDouble();
		}
		return retval;
	}

	@Override
	public String toString()
	{
		return getName();
	}
}

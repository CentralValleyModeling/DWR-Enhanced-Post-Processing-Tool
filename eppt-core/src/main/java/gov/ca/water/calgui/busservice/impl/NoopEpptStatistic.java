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
import java.util.List;
import java.util.NavigableMap;
import java.util.SortedMap;
import java.util.TreeMap;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-10-2020
 */
public class NoopEpptStatistic implements EpptStatistic
{
	@Override
	public String getName()
	{
		return "";
	}

	@Override
	public SortedMap<Month, Double> calculateMonthly(SortedMap<Month, NavigableMap<Integer, Double>> data, WaterYearDefinition waterYearDefinition,
													 WaterYearIndex waterYearIndex, List<WaterYearIndex> waterYearIndices, MonthPeriod monthPeriod)
	{
		return new TreeMap<>();
	}

	@Override
	public Double calculateYearly(SortedMap<Integer, Double> data, WaterYearDefinition waterYearDefinition, WaterYearIndex waterYearIndex,
								  List<WaterYearIndex> waterYearIndices)
	{
		return 0.0;
	}
}

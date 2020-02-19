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

package gov.ca.water.calgui.busservice.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import gov.ca.water.calgui.bo.WaterYearPeriod;

import static java.util.stream.Collectors.toList;

class WaterYearNameLookup
{


	private final Map<String, List<String>> _colHeadersToValues;

	WaterYearNameLookup(Map<String, List<String>> colHeadersToValues)
	{
		_colHeadersToValues = colHeadersToValues;
	}

	String getWaterYearType(int waterYearTypeNum, String columnHeader)
	{
		String retval = "Undefined";
		if(_colHeadersToValues.containsKey(columnHeader))
		{
			List<String> values = _colHeadersToValues.get(columnHeader);
			if(values.size() > waterYearTypeNum)
			{
				retval = values.get(waterYearTypeNum);
			}
		}
		return retval;
	}

	List<WaterYearPeriod> getSortedWaterYearPeriods(String waterYearIndexColHeader)
	{
		return _colHeadersToValues.getOrDefault(waterYearIndexColHeader, new ArrayList<>())
								  .stream()
								  .filter(Objects::nonNull)
								  .filter(s -> !s.isEmpty())
								  .filter(s->!"Undefined".equalsIgnoreCase(s))
								  .map(WaterYearPeriod::new)
								  .collect(toList());

	}
}

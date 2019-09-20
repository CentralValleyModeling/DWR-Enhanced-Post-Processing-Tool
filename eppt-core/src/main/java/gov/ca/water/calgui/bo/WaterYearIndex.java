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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-20-2019
 */
public class WaterYearIndex
{
	private final String _name;
	private final List<WaterYearType> _waterYearTypes;

	public WaterYearIndex(String name, List<WaterYearType> waterYearTypes)
	{
		_name = name;
		_waterYearTypes = waterYearTypes;
	}

	public List<WaterYearType> getWaterYearTypes()
	{
		return _waterYearTypes;
	}

	public String getName()
	{
		return _name;
	}

	private Map<WaterYearPeriod, List<WaterYearType>> getWaterYearTypeRanges()
	{
		return getWaterYearTypes()
				.stream()
				.collect(Collectors.groupingBy(WaterYearType::getWaterYearPeriod));
	}

	public Map<WaterYearPeriod, List<WaterYearPeriodRange>> getWaterYearPeriodRanges()
	{
		Map<WaterYearPeriod, List<WaterYearPeriodRange>> retval = new HashMap<>();
		for(Map.Entry<WaterYearPeriod, List<WaterYearType>> entry : getWaterYearTypeRanges().entrySet())
		{
			int start = 0;
			WaterYearPeriod key = entry.getKey();
			List<WaterYearPeriodRange> waterYearPeriodRanges = retval.computeIfAbsent(key, v -> new ArrayList<>());
			List<WaterYearType> waterYearTypes = entry.getValue();
			int startOfBlockYear = 0;
			for(int i = 1; i < waterYearTypes.size(); i++)
			{
				WaterYearType waterYearTypeStart = waterYearTypes.get(start);
				WaterYearType waterYearTypePrevious = waterYearTypes.get(i - 1);
				WaterYearType waterYearTypeCurrent = waterYearTypes.get(i);
				startOfBlockYear = waterYearTypeStart.getYear();
				int previousYear = waterYearTypePrevious.getYear();
				int currentYear = waterYearTypeCurrent.getYear();
				if(previousYear + 1 != currentYear)
				{
					if(previousYear != startOfBlockYear)
					{
						waterYearPeriodRanges.add(new WaterYearPeriodRange(key, waterYearTypeStart, waterYearTypePrevious));
					}
					start = i;
				}
			}
			WaterYearType waterYearTypeLast = waterYearTypes.get(waterYearTypes.size() - 1);
			int finalYear = waterYearTypeLast.getYear();
			if(startOfBlockYear != finalYear)
			{
				waterYearPeriodRanges.add(new WaterYearPeriodRange(key, waterYearTypes.get(start), waterYearTypeLast));
			}
			retval.put(key, waterYearPeriodRanges);
		}
		return retval;
	}

	@Override
	public String toString()
	{
		return _name.replace("index", " Index");
	}
}

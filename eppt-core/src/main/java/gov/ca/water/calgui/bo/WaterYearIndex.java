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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-20-2019
 */
public class WaterYearIndex
{
	private final List<WaterYearType> _waterYearTypes;
	private final String _name;
	private List<WaterYearPeriod> _waterYearPeriods;

	public WaterYearIndex(String name, List<WaterYearType> waterYearTypes, List<WaterYearPeriod> waterYearPeriods)
	{
		_name = name;
		_waterYearTypes = waterYearTypes;
		_waterYearTypes.sort(Comparator.comparingInt(t -> waterYearPeriods.indexOf(t.getWaterYearPeriod())));
		_waterYearPeriods = waterYearPeriods;
	}

	public List<WaterYearPeriod> getSortedPeriods()
	{
		return _waterYearPeriods;
	}

	public List<WaterYearType> getWaterYearTypes()
	{
		return _waterYearTypes;
	}

	public String getName()
	{
		return _name;
	}

	/**
	 * @return all water year type groups for all years
	 */
	public Map<WaterYearPeriod, List<WaterYearType>> getWaterYearTypeGroups()
	{
		List<WaterYearType> waterYearTypes = getWaterYearTypes();
		Map<WaterYearPeriod, List<WaterYearType>> groups = waterYearTypes
				.stream()
				.collect(groupingBy(WaterYearType::getWaterYearPeriod));
		Optional<WaterYearPeriod> dryPeriod = groups.keySet().stream().filter(WaterYearPeriod::isDry).findAny();
		Optional<WaterYearPeriod> criticalPeriod = groups.keySet().stream().filter(WaterYearPeriod::isCritical).findAny();
		if(dryPeriod.isPresent() && criticalPeriod.isPresent())
		{
			WaterYearPeriod dryAndCritical = new WaterYearPeriod("Dry & Critical");
			List<WaterYearType> dryAndCriticalTypes = new ArrayList<>();
			dryAndCriticalTypes.addAll(groups.get(dryPeriod.get()));
			dryAndCriticalTypes.addAll(groups.get(criticalPeriod.get()));
			dryAndCriticalTypes.sort(Comparator.comparingInt(WaterYearType::getYear));
			groups.put(dryAndCritical, dryAndCriticalTypes);
		}
		return groups;
	}

	/**
	 * @return all water year type groups for all years
	 */
	public Map<WaterYearPeriod, List<WaterYearPeriodRange>> getAllLongWaterYearPeriodRanges()
	{
		return getWaterYearTypes()
				.stream()
				.collect(groupingBy(WaterYearType::getWaterYearPeriod))
				.entrySet()
				.stream()
				.collect(toMap(Map.Entry::getKey,
						e -> e.getValue().stream().map(v->new WaterYearPeriodRange(v.getWaterYearPeriod(), v, v)).collect(toList())));
	}

	/**
	 * @return This method returns ranges that are two year periods or longer
	 */
	public Map<WaterYearPeriod, List<WaterYearPeriodRange>> getLongWaterYearPeriodRanges()
	{
		Map<WaterYearPeriod, List<WaterYearPeriodRange>> retval = new HashMap<>();
		for(Map.Entry<WaterYearPeriod, List<WaterYearType>> entry : getWaterYearTypeGroups().entrySet())
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
		return _name;
	}
}

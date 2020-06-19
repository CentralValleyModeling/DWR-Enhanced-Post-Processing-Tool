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

import java.time.Month;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class WaterYearPeriodRange
{
	private final WaterYearPeriod _waterYearPeriod;
	private final WaterYearType _startYear;
	private final WaterYearType _endYear;

	public WaterYearPeriodRange(WaterYearPeriod waterYearPeriod, WaterYearType startYear, WaterYearType endYear)
	{
		_waterYearPeriod = waterYearPeriod;
		_startYear = startYear;
		_endYear = endYear;
	}

	public WaterYearPeriod getWaterYearPeriod()
	{
		return _waterYearPeriod;
	}

	public WaterYearType getStartYear()
	{
		return _startYear;
	}

	public WaterYearType getEndYear()
	{
		return _endYear;
	}

	public YearMonth getStart(WaterYearDefinition waterYearDefinition)
	{
		//Checking to see if more months are in the year of the start month or the year of the end month
		int startYear = getStartYear().getYear();
		if(isCalendarEOP(waterYearDefinition) && !isCalendarBOP(waterYearDefinition))
		{
			startYear -= 1;
		}
		return YearMonth.of(startYear, waterYearDefinition.getStartMonth());
	}

	public YearMonth getEnd(WaterYearDefinition waterYearDefinition)
	{
		//Checking to see if more months are in the year of the start month or the year of the end month
		int endYear = getEndYear().getYear();
		if(!isCalendarEOP(waterYearDefinition) && isCalendarBOP(waterYearDefinition))
		{
			endYear += 1;
		}
		return YearMonth.of(endYear, waterYearDefinition.getEndMonth());
	}

	private boolean isCalendarEOP(WaterYearDefinition waterYearDefinition)
	{
		return waterYearDefinition.getStartMonth().ordinal() > waterYearDefinition.getEndMonth().ordinal()
				&& (Month.values().length - waterYearDefinition.getEndMonth().ordinal()) < waterYearDefinition.getStartMonth().ordinal();
	}

	private boolean isCalendarBOP(WaterYearDefinition waterYearDefinition)
	{
		return waterYearDefinition.getStartMonth().ordinal() > waterYearDefinition.getEndMonth().ordinal()
				&& (Month.values().length - waterYearDefinition.getEndMonth().ordinal()) > waterYearDefinition.getStartMonth().ordinal();
	}

	public String toString(WaterYearDefinition waterYearDefinition, DateTimeFormatter formatter)
	{
		if(waterYearDefinition != null)
		{

			return String.format("%s - %s", getStart(waterYearDefinition).format(formatter),
					getEnd(waterYearDefinition).format(formatter));
		}
		else
		{
			if(getStartYear().getYear() == getEndYear().getYear())
			{
				return getStartYear().getYear() + "";
			}
			else
			{
				return String.format("%s - %s", getStartYear().getYear(),
						getEndYear().getYear());
			}
		}
	}
}

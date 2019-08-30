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

public class WaterYearType
{
	private final int _year;
	private final WaterYearPeriod _waterYearType;

	public WaterYearType(int year, WaterYearPeriod waterYearPeriod)
	{
		_year = year;
		_waterYearType = waterYearPeriod;
	}

	public int getYear()
	{
		return _year;
	}

	public WaterYearPeriod getWaterYearPeriod()
	{
		return _waterYearType;
	}

}

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

package gov.ca.water.reportengine.reportreaders;

public class WaterYearType
{
	private final String[] _columnHeaders;
	private final int _year;
	private final String _waterYearType;

	public WaterYearType(String[] columnHeaders, int[] values, String waterYearType)
	{
		_columnHeaders = columnHeaders;
		_year = values[0];

		_waterYearType = waterYearType;
	}

	public int getYear()
	{
		return _year;
	}

	public String getWaterYearType()
	{
		return _waterYearType;
	}
}

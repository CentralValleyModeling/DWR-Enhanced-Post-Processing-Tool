/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.calgui.bo;

import java.util.List;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-20-2019
 */
public class WaterYearIndexModel
{
	private final List<WaterYearType> _waterYearTypes;
	private final String _name;
	private final int _waterYearIndexId;

	public WaterYearIndexModel(int waterYearIndexId, String name, List<WaterYearType> waterYearTypes)
	{
		_name = name;
		_waterYearTypes = waterYearTypes;
		_waterYearIndexId = waterYearIndexId;
	}

	public List<WaterYearType> getWaterYearTypes()
	{
		return _waterYearTypes;
	}

	public int getWaterYearIndexId()
	{
		return _waterYearIndexId;
	}

	@Override
	public String toString()
	{
		return _name;
	}
}

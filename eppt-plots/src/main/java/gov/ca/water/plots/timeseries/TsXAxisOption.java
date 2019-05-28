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

package gov.ca.water.plots.timeseries;

import com.fasterxml.jackson.annotation.JsonGetter;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */

public class TsXAxisOption
{
	private final String _axisType = "datetime";
	private final TitleOption _titleOption = new TitleOption();

	@JsonGetter("type")
	public String getAxisType()
	{
		return _axisType;
	}

	@JsonGetter("title")
	public TitleOption getTitleOption()
	{
		return _titleOption;
	}
}

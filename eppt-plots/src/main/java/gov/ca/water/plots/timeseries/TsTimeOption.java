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

import java.util.TimeZone;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonRootName;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
@JsonRootName("time")
public class TsTimeOption
{
	private TimeZone _timeZone = TimeZone.getDefault();

	@JsonGetter
	public String getTimeZone()
	{
		return _timeZone.getID();
	}

	public void setTimeZone(TimeZone timeZone)
	{
		_timeZone = timeZone;
	}
}

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

import java.util.Objects;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class WaterYearPeriod
{
	private final String _period;

	public WaterYearPeriod(String period)
	{
		_period = period;
	}

	public String getPeriodName()
	{
		return _period;
	}

	@Override
	public String toString()
	{
		return _period + " Periods";
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
		{
			return true;
		}
		if(o == null || getClass() != o.getClass())
		{
			return false;
		}
		final WaterYearPeriod that = (WaterYearPeriod) o;
		return _period.equals(that._period);
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(_period);
	}

	public boolean isDry()
	{
		return "dry".equalsIgnoreCase(_period);
	}

	public boolean isCritical()
	{
		return "critical".equalsIgnoreCase(_period);
	}
}

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

import java.time.LocalDateTime;
import java.time.Month;
import java.util.Map;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-29-2019
 */
public class MonthPeriodFilter implements PeriodFilter
{
	private final Month _month;

	public MonthPeriodFilter(Month month)
	{
		_month = month;
	}

	@Override
	public boolean test(Map.Entry<LocalDateTime, Double> input)
	{
		//Shifting the ranges because values are EOP
		LocalDateTime key = input.getKey().minusMonths(1);
		return key.getMonth().equals(_month);
	}
}

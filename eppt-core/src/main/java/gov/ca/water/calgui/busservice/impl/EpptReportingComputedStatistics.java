/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.busservice.impl;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;

import gov.ca.water.calgui.constant.Constant;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-03-2020
 */
public class EpptReportingComputedStatistics
{
	private static final String STATISTIC = "statistic";
	private static final String STATISTICALLY_AGGREGATE = "statistic_aggregate";
	private static final String STATISTICALLY_COMPUTED_MONTHLY = "statistically_computed_time_series_monthly";
	private final SortedMap<Month, Double> _statisticallyComputedMonthly;
	private final EpptStatistic _epptStatistic;
	private final Double _aggregateStatistic;

	EpptReportingComputedStatistics(EpptStatistic epptStatistic, Double aggregateStatistic,
										   SortedMap<Month, Double> statisticallyComputedMonthly)
	{
		_epptStatistic = epptStatistic;
		_aggregateStatistic = aggregateStatistic;
		_statisticallyComputedMonthly = statisticallyComputedMonthly;
	}

	public EpptStatistic getEpptStatistic()
	{
		return _epptStatistic;
	}

	public Double getAggregateStatistic()
	{
		return _aggregateStatistic;
	}

	public SortedMap<Month, Double> getStatisticallyComputedMonthly()
	{
		return _statisticallyComputedMonthly;
	}

	JSONObject toJson()
	{
		JSONObject jsonObject = new JSONObject();
		jsonObject.put(STATISTIC, _epptStatistic.getName());
		if(Constant.isValidValue(_aggregateStatistic))
		{
			jsonObject.put(STATISTICALLY_AGGREGATE, _aggregateStatistic);
		}
		jsonObject.put(STATISTICALLY_COMPUTED_MONTHLY, buildMonthMap(_statisticallyComputedMonthly));
		return jsonObject;
	}

	private JSONArray buildMonthMap(SortedMap<Month, Double> fullTimeSeries)
	{
		JSONArray jsonArray = new JSONArray();
		fullTimeSeries.entrySet()
					  .stream()
					  .filter(v->!Double.isNaN(v.getValue()))
					  .map(this::extractMonthArray)
					  .forEach(jsonArray::put);
		return jsonArray;
	}

	private JSONArray extractMonthArray(Map.Entry<Month, Double> e)
	{
		JSONArray retval = new JSONArray();
		retval.put(e.getKey().getDisplayName(TextStyle.SHORT, Locale.getDefault()));
		retval.put(e.getValue());
		return retval;
	}
}

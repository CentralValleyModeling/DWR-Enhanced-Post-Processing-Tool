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

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.TextStyle;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;

import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-26-2019
 */
public class EpptReportingComputed
{
	private static final String MONTH_PERIOD = "month_period";
	private static final String ANNUAL_PERIOD = "annual_period";
	private static final String FULL_TIME_SERIES = "discrete_ts";
	private static final String PERIOD_FILTERED_TIME_SERIES = "aggregate_ts";
	private static final String COMPUTED_STATISTICS = "computed_statistics";
	private static final String PERIOD_MONTHS = "period_months";
	private final MonthPeriod _monthPeriod;
	private final WaterYearPeriodRangesFilter _waterYearPeriodRangesFilter;
	private final SortedMap<LocalDateTime, Double> _discreteSeries;
	private final SortedMap<Integer, Double> _aggregateSeries;
	private final List<EpptReportingComputedStatistics> _computedStatistics;

	EpptReportingComputed(MonthPeriod monthPeriod, WaterYearPeriodRangesFilter waterYearPeriodRangesFilter,
						  SortedMap<LocalDateTime, Double> discreteSeries,
						  SortedMap<Integer, Double> annualPeriodFiltered,
						  List<EpptReportingComputedStatistics> computedStatistics)
	{
		_monthPeriod = monthPeriod;
		_waterYearPeriodRangesFilter = waterYearPeriodRangesFilter;
		_discreteSeries = discreteSeries;
		_aggregateSeries = annualPeriodFiltered;
		_computedStatistics = computedStatistics;
	}

	public List<EpptReportingComputedStatistics> getComputedStatistics()
	{
		return _computedStatistics;
	}

	public WaterYearPeriodRangesFilter getWaterYearPeriodRangesFilter()
	{
		return _waterYearPeriodRangesFilter;
	}

	public MonthPeriod getMonthPeriod()
	{
		return _monthPeriod;
	}

	public SortedMap<LocalDateTime, Double> getDiscreteSeries()
	{
		return _discreteSeries;
	}

	JSONObject toJson()
	{
		JSONObject jsonObject = new JSONObject();
		jsonObject.put(MONTH_PERIOD, _monthPeriod.getName());
		jsonObject.put(ANNUAL_PERIOD, _waterYearPeriodRangesFilter.getGroupName() + "<br>" + _waterYearPeriodRangesFilter.getName());
		jsonObject.put(FULL_TIME_SERIES, buildTimeSeriesMap(_discreteSeries));
		jsonObject.put(PERIOD_FILTERED_TIME_SERIES, buildYearMap(_aggregateSeries));
		jsonObject.put(COMPUTED_STATISTICS, buildComputedStatisticsArray(_computedStatistics));
		JSONArray periodMonths = new JSONArray();
		jsonObject.put(PERIOD_MONTHS, periodMonths);
		EpptReportingMonths.getMonths(_monthPeriod).forEach(e -> periodMonths.put(e.getDisplayName(TextStyle.SHORT, Locale.getDefault())));
		return jsonObject;
	}

	private JSONArray buildComputedStatisticsArray(List<EpptReportingComputedStatistics> computedStatistics)
	{
		JSONArray retval = new JSONArray();
		computedStatistics.stream()
						  .map(EpptReportingComputedStatistics::toJson)
						  .forEach(retval::put);
		return retval;
	}

	public SortedMap<Integer, Double> getAggregateSeries()
	{
		return _aggregateSeries;
	}

	private JSONArray buildTimeSeriesMap(SortedMap<LocalDateTime, Double> fullTimeSeries)
	{
		JSONArray jsonArray = new JSONArray();
		fullTimeSeries.entrySet()
					  .stream()
					  .filter(v->!Double.isNaN(v.getValue()))
					  .map(this::extractDateArray)
					  .forEach(jsonArray::put);
		return jsonArray;
	}

	private JSONArray extractDateArray(Map.Entry<LocalDateTime, Double> e)
	{
		JSONArray retval = new JSONArray();
		long l = ZonedDateTime.of(e.getKey(), ZoneId.systemDefault()).toInstant().toEpochMilli();
		retval.put(l);
		retval.put(e.getValue());
		return retval;
	}

	private JSONArray extractYearArray(Map.Entry<Integer, Double> e)
	{
		JSONArray retval = new JSONArray();
		retval.put(e.getKey());
		retval.put(e.getValue());
		return retval;
	}

	private JSONArray buildYearMap(SortedMap<Integer, Double> fullTimeSeries)
	{
		JSONArray jsonArray = new JSONArray();
		fullTimeSeries.entrySet()
					  .stream().map(this::extractYearArray)
					  .forEach(jsonArray::put);
		return jsonArray;
	}


}

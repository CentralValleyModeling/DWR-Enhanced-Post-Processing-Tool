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

package gov.ca.water.calgui.compute;

import java.time.format.TextStyle;
import java.util.List;
import java.util.Locale;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.compute.stats.Statistics;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-26-2019
 */
public class EpptReportingComputedSet
{
	private static final String SCENARIO_RUN_DATA = "scenario_run_data";
	private static final String GUI_LINK_TITLE = "gui_link_title";
	private static final String STATISTICS = "statistics";
	private static final String MONTH_PERIOD = "month_period_title";
	private static final String PERIOD_MONTHS = "period_months";
	private static final String TAF = "taf";
	private final GUILinksAllModelsBO _guiLink;
	private final Statistics _statistics;
	private final EpptReportingMonths.MonthPeriod _monthPeriod;
	private final List<EpptReportingComputed> _epptReportingComputed;
	private final boolean _taf;

	public EpptReportingComputedSet(GUILinksAllModelsBO guiLink, Statistics statistics,
									EpptReportingMonths.MonthPeriod monthPeriod, boolean taf,
									List<EpptReportingComputed> epptReportingComputed)
	{
		_guiLink = guiLink;
		_statistics = statistics;
		_monthPeriod = monthPeriod;
		_taf = taf;
		_epptReportingComputed = epptReportingComputed;
	}

	public JSONObject toJson()
	{
		JSONObject jsonObject = new JSONObject();
		JSONArray jsonArray = new JSONArray();
		jsonObject.put(GUI_LINK_TITLE, _guiLink.getPlotTitle());
		jsonObject.put(STATISTICS, _statistics.getName());
		jsonObject.put(MONTH_PERIOD, _monthPeriod.toString());
		jsonObject.put(SCENARIO_RUN_DATA, jsonArray);
		jsonObject.put(TAF, _taf);
		JSONArray periodMonths = new JSONArray();
		EpptReportingMonths.getMonths(_monthPeriod).forEach(e -> periodMonths.put(e.getDisplayName(TextStyle.SHORT, Locale.getDefault())));
		jsonObject.put(PERIOD_MONTHS, periodMonths);
		_epptReportingComputed.forEach(e -> jsonArray.put(e.toJson()));
		return jsonObject;
	}
}

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

package gov.ca.water.plotly.ui;

import java.nio.file.Path;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.plotly.PlotlyChart;
import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-23-2019
 */
public class PlotlyTimeSeriesChart extends PlotlyChart
{
	private static final Path TEMPLATE = Constant.PLOTLY_TEMPLATE_PATH.resolve("timeseries_chart.json");

	@Override
	protected Path getTemplatePath()
	{
		return TEMPLATE;
	}

	@Override
	protected JSONObject buildLayout(JSONObject template)
	{
		return template;
	}

	@Override
	protected JSONArray buildDataArray(JSONArray arrayTemplate)
	{
		return null;
	}
}

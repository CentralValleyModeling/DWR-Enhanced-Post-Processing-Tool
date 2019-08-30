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

package gov.ca.water.reportengine.standardsummary;

import java.util.Arrays;

import gov.ca.water.reportengine.EpptReportException;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public enum ChartType
{
	BASE_ALT_DIFF_TABLE("base-alt-diff-table"),
	PERCENT_DIFF_TABLE("percent-diff-table"),
	CONTROL_TABLE("control-table"),
	COA_TABLE("coa-table"),
	EXCEEDANCE_PAGE("exceedance-page"),
	EXCEEDANCE("exceedance"),
	LINE_PLOT("line-plot"),
	SCATTER_PLOT("scatter-plot"),
	LIST("list");

	private String _id;

	ChartType(String id)
	{
		_id = id;
	}

	public static ChartType getChartTypeForId(String id) throws EpptReportException
	{
		return Arrays.asList(values())
					 .stream()
					 .filter(v -> v._id.equals(id))
					 .findAny()
					 .orElseThrow(() -> new EpptReportException("Unable to find matching Chart Type: " + id));
	}

	public String getId()
	{
		return _id;
	}
}

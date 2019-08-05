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

package gov.ca.water.highcharts;

import java.nio.file.Path;
import java.nio.file.Paths;

import gov.ca.water.calgui.constant.Constant;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-05-2019
 */
public enum ChartType
{
	SCATTER(Paths.get("qaqc_scatter.html")),
	TIME_SERIES(Paths.get("")),
	BAR(Paths.get("qaqc_bar_stacked.html")),
	STACKED_BAR(Paths.get("qaqc_bar_stacked.html")),
	EXCEEDANCE(Paths.get("qaqc_exceedance.html"));

	private final Path _chartPath;

	ChartType(Path chartPath)
	{
		_chartPath = chartPath;
	}

	public Path getChartPath()
	{
		return Constant.QA_QC_CHARTS_PATH.resolve(_chartPath);
	}
}

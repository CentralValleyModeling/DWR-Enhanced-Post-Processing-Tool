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

package gov.ca.water.calgui.presentation.plotly;

import java.nio.file.Path;
import java.nio.file.Paths;

import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;

class BoxPlotChartPaneAggreate extends PlotlyPane
{

	BoxPlotChartPaneAggreate(EpptReportingComputedSet epptReportingComputedSet)
	{
		super(epptReportingComputedSet);
	}

	@Override
	Path getHtmlPath()
	{
		return Paths.get("boxandwhisker_aggregate.htm");
	}
}

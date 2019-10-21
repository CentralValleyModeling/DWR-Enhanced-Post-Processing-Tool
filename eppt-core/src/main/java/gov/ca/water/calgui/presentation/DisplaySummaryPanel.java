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

package gov.ca.water.calgui.presentation;

import java.util.List;

import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.busservice.DssGrabber2Results;
import gov.ca.water.calgui.busservice.DssGrabberResults;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel;
import gov.ca.water.calgui.presentation.display.SummaryTablePanel2;
import gov.ca.water.calgui.project.PlotConfigurationState;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 10-08-2019
 */
final class DisplaySummaryPanel
{
	private DisplaySummaryPanel()
	{
		throw new AssertionError("Utility class");
	}

	static SummaryTablePanel2 buildSummaryPanel(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
														List<String> summaryTags, DssGrabber2Results dssGrabber2Results)
	{
		TimeSeriesContainer[][] results = dssGrabber2Results.getResults();
		TimeSeriesContainer[][] diffResults = dssGrabber2Results.getDiffResults();
		SummaryTablePanel2 stp;
		if(doDifference)
		{
			stp = new SummaryTablePanel2(
					dssGrabber.getPlotTitle() + " - Difference from " + results[0][0].fileName, diffResults,
					null, summaryTags, "", null, dssGrabber, doBase, mts);
		}
		else
		{
			stp = new SummaryTablePanel2(dssGrabber.getPlotTitle(), results, null, summaryTags,
					dssGrabber.getSLabel(), null, dssGrabber, doBase, mts);
		}
		return stp;
	}

	static SummaryTablePanel insertSummaryTable(DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
												List<String> summaryTags,
												DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] secondaryResults = dssGrabberResults.getSecondaryResults();
		TimeSeriesContainer[] diffResults = dssGrabberResults.getDiffResults();
		SummaryTablePanel stp;
		if(doDifference)
		{
			stp = new SummaryTablePanel(
					dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
					diffResults, null, summaryTags, "", dssGrabber);
		}
		else
		{
			stp = new SummaryTablePanel(dssGrabber.getPlotTitle(), primaryResults, secondaryResults,
					summaryTags, dssGrabber.getSLabel(), dssGrabber, doBase);
		}
		return stp;
	}

	static SummaryTablePanel buildSummaryTablePanel(PlotConfigurationState plotConfigurationState, IDSSGrabber1Svc dssGrabber,
															DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] secondaryResults = dssGrabberResults.getSecondaryResults();
		TimeSeriesContainer[] diffResults = dssGrabberResults.getDiffResults();
		SummaryTablePanel stp;
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			stp = new SummaryTablePanel(
					dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
					diffResults, null, plotConfigurationState.getSelectedSummaryTableItems(), "", dssGrabber);
		}
		else
		{
			stp = new SummaryTablePanel(dssGrabber.getPlotTitle(), primaryResults,
					secondaryResults,
					plotConfigurationState.getSelectedSummaryTableItems(), dssGrabber.getSLabel(), dssGrabber,
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
		}
		return stp;
	}
}

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

import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.busservice.DssGrabber2Results;
import gov.ca.water.calgui.busservice.DssGrabberResults;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel;
import gov.ca.water.calgui.presentation.display.MonthlyTablePanel2;
import gov.ca.water.calgui.project.PlotConfigurationState;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 10-08-2019
 */
final class DisplayMonthlyPanel
{
	private DisplayMonthlyPanel()
	{
		throw new AssertionError("Utility class");
	}

	static MonthlyTablePanel2 buildMonthlyPanel(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
														DssGrabber2Results dssGrabber2Results)
	{
		TimeSeriesContainer[][] results = dssGrabber2Results.getResults();
		TimeSeriesContainer[][] diffResults = dssGrabber2Results.getDiffResults();
		MonthlyTablePanel2 mtp;
		if(doDifference)
		{
			mtp = new MonthlyTablePanel2(
					dssGrabber.getPlotTitle() + " - Difference from " + results[0][0].fileName, diffResults,
					dssGrabber, "", doBase, mts);
		}
		else
		{
			mtp = new MonthlyTablePanel2(dssGrabber.getPlotTitle(), results, dssGrabber,
					dssGrabber.getSLabel(),
					doBase, mts);
		}
		return mtp;
	}

	static MonthlyTablePanel buildMonthlyPanel(PlotConfigurationState plotConfigurationState, IDSSGrabber1Svc dssGrabber,
											   DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] secondaryResults = dssGrabberResults.getSecondaryResults();
		TimeSeriesContainer[] diffResults = dssGrabberResults.getDiffResults();
		MonthlyTablePanel mtp;
		if(plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.DIFF)
		{
			mtp = new MonthlyTablePanel(
					dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
					diffResults, null, dssGrabber, "");
		}
		else
		{
			mtp = new MonthlyTablePanel(dssGrabber.getPlotTitle(), primaryResults,
					secondaryResults,
					dssGrabber, dssGrabber.getSLabel(),
					plotConfigurationState.getComparisonType() == PlotConfigurationState.ComparisonType.BASE);
		}
		return mtp;
	}

	static MonthlyTablePanel buildMonthlyPanel(DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
											   DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] secondaryResults = dssGrabberResults.getSecondaryResults();
		TimeSeriesContainer[] diffResults = dssGrabberResults.getDiffResults();
		MonthlyTablePanel mtp;
		if(doDifference)
		{
			mtp = new MonthlyTablePanel(
					dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
					diffResults, null, dssGrabber, "");
		}
		else
		{
			mtp = new MonthlyTablePanel(dssGrabber.getPlotTitle(), primaryResults, secondaryResults,
					dssGrabber, dssGrabber.getSLabel(), doBase);
		}
		return mtp;
	}
}

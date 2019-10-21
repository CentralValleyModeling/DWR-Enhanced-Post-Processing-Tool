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

import javax.swing.*;

import calsim.app.MultipleTimeSeries;
import gov.ca.water.calgui.busservice.DssGrabber2Results;
import gov.ca.water.calgui.busservice.DssGrabberResults;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;
import gov.ca.water.calgui.presentation.display.ChartPanel1;
import gov.ca.water.calgui.presentation.display.ChartPanel2;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 10-08-2019
 */
final class DisplayPlotlyTimeSeries
{

	private DisplayPlotlyTimeSeries()
	{
		throw new AssertionError("Utility class");
	}

	static ChartPanel2 buildComparisonTimeSeriesPlot(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, DssGrabber2Results dssGrabber2Results)
	{
		final ChartPanel2 cp1;
		TimeSeriesContainer[][] results = dssGrabber2Results.getResults();
		cp1 = new ChartPanel2(dssGrabber.getPlotTitle() + " - Comparison ", dssGrabber.getYLabel(),
				results, false, mts);
		return cp1;
	}

	static ChartPanel2 buildDiffTimeSeriesPlot(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, DssGrabber2Results dssGrabber2Results)
	{
		final ChartPanel2 cp2;
		TimeSeriesContainer[][] results = dssGrabber2Results.getResults();
		TimeSeriesContainer[][] diffResults = dssGrabber2Results.getDiffResults();
		cp2 = new ChartPanel2(
				dssGrabber.getPlotTitle() + " - Difference from " + results[0][0].fileName,
				dssGrabber.getYLabel(), diffResults, false, mts);
		return cp2;
	}

	static ChartPanel2 buildBaseTimeSeriesPlot(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, boolean doBase,
											   DssGrabber2Results dssGrabber2Results)
	{
		final ChartPanel2 cp2;
		TimeSeriesContainer[][] results = dssGrabber2Results.getResults();
		cp2 = new ChartPanel2(dssGrabber.getPlotTitle(), dssGrabber.getYLabel(), results, false, doBase, mts);
		return cp2;
	}

	static ChartPanel1 buildComparisonTimeSeriesPlot(DSSGrabber2SvcImpl dssGrabber, DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] secondaryResults = dssGrabberResults.getSecondaryResults();
		final ChartPanel1 cp1;
		cp1 = new ChartPanel1(dssGrabber.getPlotTitle() + " - Comparison ", dssGrabber.getYLabel(),
				primaryResults, secondaryResults, false, dssGrabber.getSLabel());
		return cp1;
	}

	static ChartPanel1 buildDiffTimeSeriesPlot(DSSGrabber2SvcImpl dssGrabber, DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] diffResults = dssGrabberResults.getDiffResults();
		final ChartPanel1 cp2;
		cp2 = new ChartPanel1(
				dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
				dssGrabber.getYLabel(), diffResults, null, false, dssGrabber.getSLabel());
		return cp2;
	}

	static ChartPanel1 buildBaseTimeSeriesPlot(DSSGrabber2SvcImpl dssGrabber, DssGrabberResults dssGrabberResults, boolean doBase)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] secondaryResults = dssGrabberResults.getSecondaryResults();
		final ChartPanel1 cp2;
		cp2 = new ChartPanel1(dssGrabber.getPlotTitle(), dssGrabber.getYLabel(), primaryResults,
				secondaryResults, false, dssGrabber.getSLabel(), doBase);
		return cp2;
	}

	static ChartPanel1 buildComparisonTimeSeriesPanel(IDSSGrabber1Svc dssGrabber, DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] secondaryResults = dssGrabberResults.getSecondaryResults();
		final ChartPanel1 cp1;
		cp1 = new ChartPanel1(dssGrabber.getPlotTitle() + " - Comparison ",
				dssGrabber.getYLabel(),
				primaryResults, secondaryResults, false,
				dssGrabber.getSLabel());
		return cp1;
	}

	static ChartPanel1 buildDiffTimeSeriesPanel(IDSSGrabber1Svc dssGrabber, DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] diffResults = dssGrabberResults.getDiffResults();
		final ChartPanel1 cp2;
		cp2 = new ChartPanel1(
				dssGrabber.getPlotTitle() + " - Difference from " + primaryResults[0].fileName,
				dssGrabber.getYLabel(), diffResults, null, false,
				dssGrabber.getSLabel());
		return cp2;
	}

	static ChartPanel1 buildBaseTimeSeriesPanel(IDSSGrabber1Svc dssGrabber, DssGrabberResults dssGrabberResults)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[] secondaryResults = dssGrabberResults.getSecondaryResults();
		final ChartPanel1 cp2;
		cp2 = new ChartPanel1(dssGrabber.getPlotTitle(), dssGrabber.getYLabel(),
				primaryResults,
				secondaryResults, false, dssGrabber.getSLabel(), true);
		return cp2;
	}
}

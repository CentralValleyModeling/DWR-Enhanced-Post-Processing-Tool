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
final class DisplayPlotlyExceedance
{
	private DisplayPlotlyExceedance()
	{
		throw new AssertionError("Utility class");
	}

	static ChartPanel1 buildExceedance(IDSSGrabber1Svc dssGrabber, boolean doDifference, boolean doBase,
									   DssGrabberResults dssGrabberResults,
									   String s, String s2, int i2,
									   String s3)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[][] dexcResults = dssGrabberResults.getDexcResults();
		TimeSeriesContainer[][] excResults = dssGrabberResults.getExcResults();
		TimeSeriesContainer[][] sexcResults = dssGrabberResults.getSexcResults();
		ChartPanel1 cp3 = null;
		if(doDifference && dexcResults != null)
		{
			cp3 = new ChartPanel1(
					dssGrabber.getPlotTitle() + s
							+ " - Difference from " + primaryResults[0].fileName,
					s2, dexcResults[i2], null, true, dssGrabber.getSLabel());
		}
		else if(excResults != null)
		{
			cp3 = new ChartPanel1(
					s3,
					s2, excResults[i2],
					sexcResults == null ? null : sexcResults[i2], true, dssGrabber.getSLabel(), doBase);
		}
		return cp3;
	}

	static ChartPanel1 buildExceedanceForMonth(boolean doDifference, boolean doBase,
											   DssGrabberResults dssGrabberResults, int m1, String monthName,
											   String plotTitle, String yLabel, String sLabel)
	{
		TimeSeriesContainer[] primaryResults = dssGrabberResults.getPrimaryResults();
		TimeSeriesContainer[][] dexcResults = dssGrabberResults.getDexcResults();
		TimeSeriesContainer[][] excResults = dssGrabberResults.getExcResults();
		TimeSeriesContainer[][] sexcResults = dssGrabberResults.getSexcResults();
		final ChartPanel1 cp3;
		if(doDifference)
		{
			cp3 = new ChartPanel1(
					plotTitle + " - Exceedance (" + monthName + ")"
							+ " - Difference from " + primaryResults[0].fileName,
					yLabel, dexcResults[m1], null, true, sLabel);
		}
		else
		{
			cp3 = new ChartPanel1(
					plotTitle + " - Exceedance (" + monthName + ")",
					yLabel, excResults[m1],
					sexcResults == null ? null : sexcResults[m1], true, sLabel, doBase);
		}
		return cp3;
	}

	static ChartPanel2 buildExceedanceForMonth(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
											   String[] monthNames, DssGrabber2Results dssGrabber2Results, int m1)
	{
		TimeSeriesContainer[][] results = dssGrabber2Results.getResults();
		TimeSeriesContainer[][][] dexcResults = dssGrabber2Results.getDexcResults();
		TimeSeriesContainer[][][] excResults = dssGrabber2Results.getExcResults();
		final ChartPanel2 cp3;
		if(doDifference)
		{
			cp3 = new ChartPanel2(
					dssGrabber.getPlotTitle() + " - Exceedance (" + monthNames[m1] + ")"
							+ " - Difference from " + results[0][0].fileName,
					dssGrabber.getYLabel(), dexcResults[m1], true, doBase, mts);
		}
		else
		{
			cp3 = new ChartPanel2(
					dssGrabber.getPlotTitle() + " - Exceedance (" + monthNames[m1] + ")",
					dssGrabber.getYLabel(), excResults[m1], true, doBase, mts);
		}
		return cp3;
	}

	static ChartPanel2 buildAllExceedance(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, boolean doDifference,
										 DssGrabber2Results dssGrabber2Results)
	{
		TimeSeriesContainer[][] results = dssGrabber2Results.getResults();
		TimeSeriesContainer[][][] dexcResults = dssGrabber2Results.getDexcResults();
		TimeSeriesContainer[][][] excResults = dssGrabber2Results.getExcResults();
		final ChartPanel2 cp3;
		if(doDifference)
		{
			cp3 = new ChartPanel2(
					dssGrabber.getPlotTitle() + " - Exceedance (all months)" + " - Difference from "
							+ results[0][0].fileName,
					dssGrabber.getYLabel(), dexcResults[13], true, mts);
		}
		else
		{
			cp3 = new ChartPanel2(dssGrabber.getPlotTitle() + " - Exceedance (all months)",
					dssGrabber.getYLabel(), excResults[13], true, mts);
		}
		return cp3;
	}

	static ChartPanel2 buildAnnualExceedance(MultipleTimeSeries mts, DSSGrabber2SvcImpl dssGrabber, boolean doDifference, boolean doBase,
											 DssGrabber2Results dssGrabber2Results)
	{
		TimeSeriesContainer[][] results = dssGrabber2Results.getResults();
		TimeSeriesContainer[][][] dexcResults = dssGrabber2Results.getDexcResults();
		TimeSeriesContainer[][][] excResults = dssGrabber2Results.getExcResults();
		final ChartPanel2 cp3;
		if(doDifference)
		{
			cp3 = new ChartPanel2(
					dssGrabber.getPlotTitle() + " - Exceedance (annual total)" + " - Difference from "
							+ results[0][0].fileName,
					"Annual Total Volume (TAF)", dexcResults[12], true, mts);
		}
		else

		{
			cp3 = new ChartPanel2(dssGrabber.getPlotTitle() + " - Exceedance (Annual Total)",
					"Annual Total Volume (TAF)", excResults[12], true, doBase, mts);
		}
		return cp3;
	}
}

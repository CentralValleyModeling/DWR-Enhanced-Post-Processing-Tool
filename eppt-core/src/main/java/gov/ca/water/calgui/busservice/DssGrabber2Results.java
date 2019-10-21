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

package gov.ca.water.calgui.busservice;

import gov.ca.water.calgui.busservice.impl.DSSGrabber2SvcImpl;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 10-08-2019
 */
public class DssGrabber2Results
{
	private final TimeSeriesContainer[][] _results;
	private final TimeSeriesContainer[][] _diffResults;
	private final TimeSeriesContainer[][][] _excResults;
	private final TimeSeriesContainer[][][] _dexcResults;

	public DssGrabber2Results(DSSGrabber2SvcImpl dssGrabber, int n, int s)
	{
		_results = new TimeSeriesContainer[n][s];
		for(int i = 0; i < n; i++)
		{
			_results[i] = dssGrabber.getMultipleTimeSeries(i);
		}

		dssGrabber.calcTAFforCFS(_results);

		_diffResults = dssGrabber.getDifferenceSeriesWithMultipleTimeSeries(_results);
		_excResults = dssGrabber.getExceedanceSeriesWithMultipleTimeSeries(_results);
		_dexcResults = dssGrabber.getExceedanceSeriesDWithMultipleTimeSeries(_results);
	}

	public TimeSeriesContainer[][] getResults()
	{
		return _results;
	}

	public TimeSeriesContainer[][] getDiffResults()
	{
		return _diffResults;
	}

	public TimeSeriesContainer[][][] getExcResults()
	{
		return _excResults;
	}

	public TimeSeriesContainer[][][] getDexcResults()
	{
		return _dexcResults;
	}
}

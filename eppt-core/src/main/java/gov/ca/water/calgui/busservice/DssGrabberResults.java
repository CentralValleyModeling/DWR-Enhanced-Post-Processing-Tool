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

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 10-08-2019
 */
public class DssGrabberResults
{
	private final TimeSeriesContainer[] _primaryResults;
	private final TimeSeriesContainer[] _secondaryResults;
	private final TimeSeriesContainer[] _diffResults;
	private final TimeSeriesContainer[][] _excResults;
	private final TimeSeriesContainer[][] _sexcResults;
	private final TimeSeriesContainer[][] _dexcResults;

	public DssGrabberResults(IDSSGrabber1Svc dssGrabber)
	{
		_primaryResults = dssGrabber.getPrimarySeries();
		_secondaryResults = dssGrabber.getSecondarySeries();


		dssGrabber.calcTAFforCFS(_primaryResults, _secondaryResults);

		_diffResults = dssGrabber.getDifferenceSeries(_primaryResults);
		_excResults = dssGrabber.getExceedanceSeries(_primaryResults);
		_sexcResults = dssGrabber.getExceedanceSeries(_secondaryResults);
		_dexcResults = dssGrabber.getExceedanceSeriesD(_primaryResults);
	}

	public TimeSeriesContainer[] getPrimaryResults()
	{
		return _primaryResults;
	}

	public TimeSeriesContainer[] getSecondaryResults()
	{
		return _secondaryResults;
	}

	public TimeSeriesContainer[] getDiffResults()
	{
		return _diffResults;
	}

	public TimeSeriesContainer[][] getExcResults()
	{
		return _excResults;
	}

	public TimeSeriesContainer[][] getSexcResults()
	{
		return _sexcResults;
	}

	public TimeSeriesContainer[][] getDexcResults()
	{
		return _dexcResults;
	}
}

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

package gov.ca.water.reportengine.executivereport;

import java.util.ArrayList;
import java.util.List;

import hec.heclib.util.HecTime;
import hec.lang.Const;

public class FlagViolation
{

	private final List<HecTime> _times = new ArrayList<>();
	private final String _dtsFileName;
	private final double _maxValue;

	FlagViolation(List<HecTime> times, String dtsFileName)
	{
		_times.addAll(times);
		_dtsFileName = dtsFileName;
		_maxValue = Const.UNDEFINED_DOUBLE;

	}

	FlagViolation(double maxValue, String dtsFileName)
	{
		_maxValue = maxValue;
		_dtsFileName = dtsFileName;

	}

	public String getDtsFileName()
	{
		return _dtsFileName;
	}

	public List<HecTime> getTimes()
	{
		return _times;
	}

	double getMaxValue()
	{
		return _maxValue;
	}

}

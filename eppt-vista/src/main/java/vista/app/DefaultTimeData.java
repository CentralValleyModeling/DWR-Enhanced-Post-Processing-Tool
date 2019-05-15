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
package vista.app;

import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;

/**
   * 
   */
public class DefaultTimeData implements TimeData {
	/**
    *
    */
	public DefaultTimeData(String startTime, String timeInterval) {
		_stime = TimeFactory.getInstance().createTime(startTime);
		_ti = TimeFactory.getInstance().createTimeInterval(timeInterval);
		_first = true;
	}

	/**
	 * get next time value string
	 */
	public String getNextValue() {
		if (_first)
			_first = false;
		else
			_stime.incrementBy(_ti);
		return _stime.toString();
	}

	private Time _stime;
	private TimeInterval _ti;
	private boolean _first;
}

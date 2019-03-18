/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

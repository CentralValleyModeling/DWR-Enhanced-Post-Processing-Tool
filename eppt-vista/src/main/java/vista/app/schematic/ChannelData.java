/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.schematic;

import vista.db.dss.DSSUtil;
import vista.set.DataReference;
import vista.set.DataSetIterator;
import vista.set.Group;
import vista.set.Pathname;
import vista.set.RegularTimeSeries;
import vista.set.TimeSeriesMath;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
   * 
   */
public class ChannelData {
	/**
    * 
    */
	public ChannelData(int channelNumber, String filename, String pathname,
			String startTime, String timeInterval) {
		_filename = filename;
		_pathname = pathname;
		_stime = TimeFactory.getInstance().createTime(startTime);
		_ti = TimeFactory.getInstance().createTimeInterval(timeInterval);
		getNextChunkOfData();
		_channelId = channelNumber;
	}

	/**
    *
    */
	public int getChannelId() {
		return _channelId;
	}

	/**
    * 
    */
	public float getNextValue() {
		if (_iterator == null)
			return 0;
		if (_iterator.atEnd())
			getNextChunkOfData();
		if (_iterator == null)
			return 0;
		//
		float val = (float) _iterator.getElement().getY();
		_iterator.advance();
		return val;
	}

	/**
    * 
    */
	private void getNextChunkOfData() {
		if (_currentTimeWindow == null) {
			_currentTimeWindow = TimeFactory.getInstance().createTimeWindow(
					_stime, _stime.__add__("1YEAR"));
		} else {
			Time st = _currentTimeWindow.getStartTime();
			Time et = _currentTimeWindow.getEndTime();
			if (et.compare(_stime) < 0) { // if beyond end of time window...
				_currentTimeWindow = TimeFactory.getInstance()
						.createTimeWindow(_stime, _stime.__add__("1YEAR"));
			}
		}
		if (_reference == null) {
			_pathname = _pathname.replace('+', '.');
			Pathname path = Pathname.createPathname(_pathname);
			path.setPart(Pathname.D_PART, ".*");
			Group g = DSSUtil.createGroup("local", _filename);
			g.filterBy(path.toString());
			if (g.getNumberOfDataReferences() == 0) {
				throw new RuntimeException("No data found for "
						+ path.toString());
			} else if (g.getNumberOfDataReferences() > 1) {
				throw new RuntimeException("More than one data set found for "
						+ path.toString());
			}
			_reference = g.getDataReference(0);
		}
		//
		DataReference ref = DataReference
				.create(_reference, _currentTimeWindow);
		if (ref == null) {
			_iterator = null;
			return;
		}
		RegularTimeSeries ds = (RegularTimeSeries) ref.getData();
		if (ds.getTimeInterval().compare(_ti) < 0) {
			ds = TimeSeriesMath.doPeriodOperation(ds, _ti,
					TimeSeriesMath.PERIOD_AVERAGE);
		} else if (ds.getTimeInterval().compare(_ti) > 0) {
			ds = null;
		}
		if (ds != null)
			_iterator = ds.getIterator();
		else
			_iterator = null;
	}

	private String _filename, _pathname;
	private DataReference _reference;
	private int _channelId;
	DataSetIterator _iterator;
	String _timeWindow;
	TimeWindow _currentTimeWindow;
	Time _stime;
	TimeInterval _ti;
	private Group _group;
}

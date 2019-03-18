/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import vista.time.TimeFactory;
import vista.time.TimeWindow;

/**
 * 
 * @author Nicky Sandhu
 * @version $Id: DefaultReference.java,v 1.5 2000/03/27 18:38:42 nsandhu Exp $
 */
public class DefaultReference extends DataReference {
	/**
   *
   */
	public DefaultReference(DataSet ds) {
		String pathname = null;
		DataSetAttr attr = ds.getAttributes();
		if (ds instanceof RegularTimeSeries) {
			RegularTimeSeries rts = (RegularTimeSeries) ds;
			setName(ds.getName());
			pathname = "/" + attr.getGroupName() + "/" + attr.getLocationName()
					+ "/" + attr.getTypeName() + "/"
					+ rts.getTimeWindow().getStartTime().toString() + "/"
					+ rts.getTimeInterval().toString() + "/"
					+ attr.getSourceName() + "/";
		} else if (ds instanceof IrregularTimeSeries) {
			setName(ds.getName());
			IrregularTimeSeries its = (IrregularTimeSeries) ds;
			pathname = "/" + attr.getGroupName() + "/" + attr.getLocationName()
					+ "/" + attr.getTypeName() + "/"
					+ its.getTimeWindow().getStartTime().toString()
					+ "/IR-YEAR/" + attr.getSourceName() + "/";
		} else if (ds instanceof DefaultDataSet) {
			setName(ds.getName());
			pathname = "/" + attr.getGroupName() + "/" + attr.getLocationName()
					+ "/" + attr.getTypeName() + "///" + attr.getSourceName()
					+ "/";
		} else {
			throw new RuntimeException("Unrecognized type of data set: " + ds);
		}
		init("local", "data.dss", pathname, ds);
	}

	/**
   *
   */
	public DefaultReference(String server, String file, String path, DataSet ds) {
		init(server, file, path, ds);
	}

	/**
   *
   */
	private void init(String server, String file, String path, DataSet ds) {
		setServername(server);
		setFilename(file);
		Pathname pathname = Pathname.createPathname(path);
		setPathname(pathname);
		int dataType = 0;
		String xUnits = "TIME";
		String yUnits = "";
		String xType = "";
		String yType = "";
		if (ds instanceof RegularTimeSeries) {
			RegularTimeSeries rts = (RegularTimeSeries) ds;
			// always set the time interval before setting the timewindow..
			setTimeInterval(rts.getTimeInterval());
			super.setTimeWindow(rts.getTimeWindow());
			dataType = DataType.REGULAR_TIME_SERIES;
			xUnits = ds.getAttributes().getXUnits();
			yUnits = ds.getAttributes().getYUnits();
			xType = ds.getAttributes().getXType();
			yType = ds.getAttributes().getYType();
		} else if (ds instanceof IrregularTimeSeries) {
			IrregularTimeSeries its = (IrregularTimeSeries) ds;
			// always set the time interval before setting the timewindow..
			setTimeInterval(TimeFactory.getInstance().createTimeInterval(
					"1YEAR"));
			super.setTimeWindow(its.getTimeWindow());
			dataType = DataType.IRREGULAR_TIME_SERIES;
			xUnits = ds.getAttributes().getXUnits();
			yUnits = ds.getAttributes().getYUnits();
			xType = ds.getAttributes().getXType();
			yType = ds.getAttributes().getYType();
		} else {
			dataType = DataType.PAIRED;
			xUnits = ds.getAttributes().getXUnits();
			yUnits = ds.getAttributes().getYUnits();
			xType = ds.getAttributes().getXType();
			yType = ds.getAttributes().getYType();
		}
		DataSetAttr attr = new DataSetAttr(pathname.getPart(Pathname.A_PART),
				pathname.getPart(Pathname.B_PART), pathname
						.getPart(Pathname.C_PART), pathname
						.getPart(Pathname.F_PART), dataType, xUnits, yUnits,
				xType, yType);
		_dataSet = ds;
	}

	/**
	 * reloads data
	 */
	public void reloadData() {
	}

	/**
	 * Retrieves data from the data base if data is null. If retrieval fails it
	 * throws a RuntimeException.
	 * 
	 * @return reference to the initialized data set.
	 */
	public DataSet getData() {
		return _dataSet;
	}

	/**
	 * create a clone of itself
	 */
	protected DataReference createClone() {
		return new DefaultReference(getServername(), getFilename(),
				getPathname().toString(), _dataSet);
	}

	/**
	 * checks equivalency of two data references.
	 */
	public boolean isSameAs(DataReference ref) {
		return (ref != null)
				&& ((ref.getTimeWindow() == null && getTimeWindow() == null) || (ref
						.getTimeWindow() != null && ref.getTimeWindow().equals(
						getTimeWindow())))
				&& ((ref.getTimeInterval() == null && getTimeInterval() == null) || (ref
						.getTimeInterval() != null && ref.getTimeInterval()
						.equals(getTimeInterval())))
				&& (ref.getFilename().equals(getFilename()))
				&& (ref.getServername().equals(getServername()))
				&& (ref.getPathname().equals(getPathname()));
	}

	/**
	 * gets the time window for this reference
	 */
	protected void setTimeWindow(TimeWindow tw) {
		super.setTimeWindow(tw);
		if (tw == null)
			return;
		if (!(_dataSet instanceof TimeSeries))
			return;
		if (_dataSet instanceof RegularTimeSeries)
			_dataSet = ((RegularTimeSeries) _dataSet)
					.createSlice(getTimeWindow());
		else if (_dataSet instanceof IrregularTimeSeries)
			_dataSet = ((IrregularTimeSeries) _dataSet)
					.createSlice(getTimeWindow());
	}

	/**
   *
   */
	private DataSet _dataSet;
}

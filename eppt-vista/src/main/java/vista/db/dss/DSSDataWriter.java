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
package vista.db.dss;

import vista.set.DataSet;
import vista.set.DataSetAttr;
import vista.set.SetUtils;

import hec.heclib.util.Heclib;
import hec.heclib.util.stringContainer;

/**
 * 
 * The class with native function calls to the HEC-DSS library. Only limited
 * functionality of retrieving data is available at this time. Storing data
 * options will be added later.
 * 
 * Does not allow multithreaded access. However multi-user access is allowed.
 * 
 * @author Nicky Sandhu
 * @version $Id: DSSDataWriter.java,v 1.1 2003/10/02 20:48:45 redwood Exp $
 */
public class DSSDataWriter {
	public static final boolean DOUBLE_PRECISION = true;
	private String filename;
	private int[] ifltab;
	private int[] nvals;

	/**
    *
    */
	public DSSDataWriter(String filename) {
		this.filename = filename;
	}

	/**
   *
   */
	public void storeData(String pathname, long startJulmin, long endJulmin,
			DataSet ds, boolean storeFlags) {
		DataSetAttr attr = ds.getAttributes();
		DSSData data = new DSSData();
		int dataType = attr.getType();
		data._dataType = dataType;
		data._xType = attr.getXType();
		data._yType = attr.getYType();
		data._xUnits = attr.getXUnits();
		data._yUnits = attr.getYUnits();
		data._numberRead = ds.size();
		data._yValues = SetUtils.createYArray(ds);
		if (pathname.length() >= 1800)
			throw new IllegalArgumentException("Pathname: " + pathname
					+ " is too long (>1800chars)");
		if (dataType == DSSUtil.REGULAR_TIME_SERIES) {
			if (storeFlags)
				data._flags = SetUtils.createFlagArray(ds);
			storeTimeSeriesData(pathname, startJulmin, data,
					storeFlags);
		} else if (dataType == DSSUtil.IRREGULAR_TIME_SERIES) {
			data._xValues = SetUtils.createXArray(ds);
			if (storeFlags)
				data._flags = SetUtils.createFlagArray(ds);
			storeIrregularTimeSeriesData(pathname, 0, 0, data, storeFlags);
		} else if (dataType == DSSUtil.PAIRED) {
			data._xValues = SetUtils.createXArray(ds);
			storePairedData(pathname, data, storeFlags);
		} else {
			throw new IllegalArgumentException("Data type: " + dataType
					+ " is invalid");
		}
	}

	public void openDSSFile() {
		if (ifltab == null) {
			ifltab = DSSUtil.openDSSFile(filename, true);
		}
	}

	public void closeDSSFile() {
		if (ifltab != null) {
			DSSUtil.closeDSSFile(ifltab);
		}
	}

	/**
	 * time series data
	 */
	public synchronized void storeTimeSeriesData(String pathname,
			long startJulmin, DSSData data, boolean storeFlags) {
		int idate = (int) startJulmin / 1440;
		int startTime = (int) startJulmin % 1440;
		String startDate = Heclib.juldat(idate, 104);
		stringContainer hourMinutes = new stringContainer();
		Heclib.m2ihm(startTime, hourMinutes);
		int istoreFlags = storeFlags ? 1 : 0;
		int[] flags = storeFlags ? data._flags : new int[1];
		int[] userHeader = new int[] { 0 };
		int numberHeader = 0;
		// TODO: figure out values for compression, plan, etc.
		int plan = 0;
		int compression = 0;
		int baseValueSet = 0;
		float baseValue = 0;
		int highDelta = 0;
		int delatPrecision = 0;
		int[] status = new int[] { 0 };
		if (DOUBLE_PRECISION) {
			Heclib.zsrtsxd(ifltab, pathname, startDate, hourMinutes.toString(),
					data._numberRead, data._yValues, flags, istoreFlags,
					data._yUnits, data._yType, userHeader, numberHeader, plan,
					compression, baseValue, baseValueSet, highDelta,
					delatPrecision, status);
		} else {
			float[] yvalues = extractYValuesAsFloat(data);
			Heclib.zsrtsx(ifltab, pathname, startDate, hourMinutes.toString(),
					data._numberRead, yvalues, flags, istoreFlags,
					data._yUnits, data._yType, userHeader, numberHeader, plan,
					compression, baseValue, baseValueSet, highDelta,
					delatPrecision, status);

		}
		//TODO: check status flag and throw exception
	}

	public float[] extractYValuesAsFloat(DSSData data) {
		float[] yvalues = new float[data._numberRead];
		for (int i = 0; i < yvalues.length; i++) {
			yvalues[i] = (float) data._yValues[i];
		}
		return yvalues;
	}

	/**
	 * store irregular time series
	 */
	public synchronized void storeIrregularTimeSeriesData(String pathname,
			long startJulmin, long endJulmin, DSSData data, boolean storeFlags) {
		double[] values = data._yValues;
		int[] times = new int[data._numberRead];
		for (int i = 0; i < data._numberRead; i++) {
			times[i] = (int) data._xValues[i];
		}
		int numberVals = data._numberRead;
		int startDate = (int) startJulmin / 1440;
		int[] flags = data._flags;
		int storeFlagData = 0;
		if (storeFlags) {
			flags = data._flags;
			storeFlagData = 1;
			if (flags == null) {
				flags = new int[data._numberRead];
			}
		} else {
			storeFlagData = 0;
			flags = new int[] { 0 };
		}
		String unitsX = data._yUnits;
		String typeX = data._yType;
		int[] userHeader = new int[] { 0 };
		int numberHeader = 0;
		int inFlag = 0; // 1 is to replace data not merge data
		int[] status = new int[1];
		if (DOUBLE_PRECISION) {
			Heclib.zsitsxd(ifltab, pathname, times, values, numberVals,
					startDate, flags, storeFlagData, unitsX, typeX, userHeader,
					numberHeader, inFlag, status);
		} else {
			float[] yvalues = extractYValuesAsFloat(data);
			Heclib.zsitsx(ifltab, pathname, times, yvalues, numberVals,
					startDate, flags, storeFlagData, unitsX, typeX, userHeader,
					numberHeader, inFlag, status);

		}
	}

	/**
	 * store paired data
	 */
	public synchronized void storePairedData(String pathname, DSSData data,
			boolean storeFlags) {
		throw new RuntimeException("Not yet implemented!");
	}

	private static final boolean DEBUG = false;
}

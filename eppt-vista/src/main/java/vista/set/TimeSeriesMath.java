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
package vista.set;

import java.util.Random;

import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: TimeSeriesMath.java,v 1.1 2003/10/02 20:49:33 redwood Exp $
 */
/*
 * To be done Cannot handle regular to irregular conversions regular +,-,*,/
 * irregular == irregular unit conversions time series offset by a fraction of
 * time interval == irregular time series additions based on interpolated values
 * ARIMA, ARMA models Digital filters FFT
 */
public class TimeSeriesMath {
	public static final int ADD = 1;
	public static final int SUB = 2;
	public static final int MUL = 3;
	public static final int DIV = 4;
	public static final int PERIOD_AVERAGE = 5;
	public static final int PERIOD_MIN = 6;
	public static final int PERIOD_MAX = 7;
	public static final int PERIOD_LAST_AVERAGE = 7;
	public static final int PERIOD_LAST_MAX = 7;
	public static final int PERIOD_LAST_MIN = 7;
	public static final String ADD_STR = "add";
	public static final String SUB_STR = "subtract";
	public static final String MUL_STR = "multiply";
	public static final String DIV_STR = "divide";
	public static final String PERIOD_AVERAGE_STR = "period-average";
	public static final String PERIOD_MIN_STR = "period-min";
	public static final String PERIOD_MAX_STR = "period-max";
	public static final String PERIOD_LAST_AVERAGE_STR = "period-last-average";
	public static final String PERIOD_LAST_MIN_STR = "period-last-min";
	public static final String PERIOD_LAST_MAX_STR = "period-last-max";
	public static final int LAST_VAL = 100;
	public static final int LINEAR = 200;
	public static boolean DUMB_PATCH = false;
	private static ElementFilter _filter = Constants.DEFAULT_FLAG_FILTER;

	/**
	 * set the filter to be used for determining quality of data before using it
	 * any of the time series operations
	 */
	public static void setOperationFilter(ElementFilter f) {
		_filter = f;
	}

	/**
	 * get the filter to be used for determining quality of data before using it
	 * any of the time series operations
	 * 
	 */
	public static ElementFilter getOperationFilter() {
		return _filter;
	}

	/**
	 * get the display name for this operation
	 */
	public static String getOperationName(int i) {
		if (i == ADD) {
			return ADD_STR;
		} else if (i == SUB) {
			return SUB_STR;
		} else if (i == MUL) {
			return MUL_STR;
		} else if (i == DIV) {
			return DIV_STR;
		} else if (i == PERIOD_AVERAGE) {
			return PERIOD_AVERAGE_STR;
		} else if (i == PERIOD_MIN) {
			return PERIOD_MIN_STR;
		} else if (i == PERIOD_MAX) {
			return PERIOD_MAX_STR;
		} else if (i == PERIOD_LAST_AVERAGE) {
			return PERIOD_LAST_AVERAGE_STR;
		} else if (i == PERIOD_LAST_MAX) {
			return PERIOD_LAST_MAX_STR;
		} else if (i == PERIOD_LAST_MIN) {
			return PERIOD_LAST_MIN_STR;
		} else {
			throw new IllegalArgumentException("No operation for identifier "
					+ i);
		}

	}

	/**
   *
   */
	static RegularTimeSeries[] createCompatible(RegularTimeSeries d1,
			RegularTimeSeries d2) {
		DataSetAttr attr1 = d1.getAttributes();
		DataSetAttr attr2 = d2.getAttributes();
		if (attr1 == null || attr2 == null) {
		} else {
			if (!attr1.getYType().equals(attr2.getYType())) {
				// if (!DUMB_PATCH)
				// throw new IllegalArgumentException("Incompatible types: "+
				// attr1.getYType() +
				// " & " + attr2.getYType());
			}
			if (DUMB_PATCH) {
				// if ( ! attr1.getYUnits().equals(attr2.getYUnits() ) ){
				// doUnitConversion(d2,attr1.getYUnits());
				// }
			}
		}
		TimeInterval ti1 = d1.getTimeInterval();
		TimeInterval ti2 = d2.getTimeInterval();
		TimeInterval ti;
		if (DUMB_PATCH) {
			if (ti1.compare(ti2) < 0)
				ti = ti2;
			else
				ti = ti1;
			if (ti1.compare(ti) != 0)
				d1 = doPeriodOperation(d1, ti, PERIOD_AVERAGE);
			if (ti2.compare(ti) != 0)
				d2 = doPeriodOperation(d2, ti, PERIOD_AVERAGE);
		} else {
			if (ti1.compare(ti2) != 0) {
				throw new IllegalArgumentException("Time interval mismatch: "
						+ ti1 + " & " + ti2);
			} else {
				ti = ti1;
			}
		}
		TimeWindow tw1 = d1.getTimeWindow();
		TimeWindow tw2 = d2.getTimeWindow();
		TimeWindow tw = tw1.intersection(tw2);
		tw = TimeFactory.createRoundedTimeWindow(tw, ti);
		if (tw == null)
			throw new IllegalArgumentException("No common time window " + tw1
					+ " , " + tw2 + " for time series operation");
		d1 = (RegularTimeSeries) d1.createSlice(tw);
		d2 = (RegularTimeSeries) d2.createSlice(tw);
		return new RegularTimeSeries[] { d1, d2 };
	}

	/**
   *
   */
	static DataSetAttr createAttributes(TimeSeries d1, TimeSeries d2) {
		DataSetAttr attr1 = d1.getAttributes();
		DataSetAttr attr2 = d2.getAttributes();
		if (attr1 == null)
			return null;
		DataSetAttr attr = new DataSetAttr(attr1.getGroupName(), attr1
				.getLocationName()
				+ " , " + attr2.getLocationName(), attr1.getTypeName(), attr1
				.getSourceName(), attr1.getType(), attr1.getXUnits(), attr1
				.getYUnits(), attr1.getXType(), attr1.getYType());
		return attr;
	}

	/**
   *
   */
	static DataSetAttr createAttributes(TimeSeries d1, TimeSeries d2, int opId) {
		DataSetAttr attr1 = d1.getAttributes();
		DataSetAttr attr2 = d2.getAttributes();
		if (attr1 == null)
			return null;
		DataSetAttr attr = new DataSetAttr(attr1.getGroupName(), attr1
				.getLocationName()
				+ " " + getOperationName(opId) + " " + attr2.getLocationName(),
				attr1.getTypeName(), attr1.getSourceName(), attr1.getType(),
				attr1.getXUnits(), attr1.getYUnits(), attr1.getXType(), attr1
						.getYType());
		return attr;
	}

	/**
   *
   */
	static DataSetAttr createAttributes(TimeSeries d1, double val, int opId) {
		DataSetAttr attr1 = d1.getAttributes();
		if (attr1 == null)
			return null;
		DataSetAttr attr = new DataSetAttr(attr1.getGroupName(), attr1
				.getLocationName()
				+ " " + getOperationName(opId) + " " + val,
				attr1.getTypeName(), attr1.getSourceName(), attr1.getType(),
				attr1.getXUnits(), attr1.getYUnits(), attr1.getXType(), attr1
						.getYType());
		return attr;
	}

	/**
   *
   */
	static DataSetAttr createAttributes(TimeSeries d1, double[] val, int opId) {
		DataSetAttr attr1 = d1.getAttributes();
		if (attr1 == null)
			return null;
		String str = "[";
		for (int i = 0; i < Math.min(val.length, 3); i++)
			str = str + val[i] + ",";
		str = str + "...]";
		DataSetAttr attr = new DataSetAttr(attr1.getGroupName(), attr1
				.getLocationName()
				+ " " + getOperationName(opId) + str, attr1.getTypeName(),
				attr1.getSourceName(), attr1.getType(), attr1.getXUnits(),
				attr1.getYUnits(), attr1.getXType(), attr1.getYType());
		return attr;
	}

	/**
   *
   */
	static RegularTimeSeries doBinaryOperation(RegularTimeSeries d1,
			RegularTimeSeries d2, int operationId) {
		RegularTimeSeries[] d12 = createCompatible(d1, d2);
		d1 = d12[0];
		d2 = d12[1];
		int size = d1.size();
		int index = 0;
		double[] y = new double[size];
		int[] flags = null;
		if (d1.isFlagged() || d2.isFlagged()) {
			flags = new int[size];
		}
		DataSetIterator dsi1 = d1.getIterator();
		DataSetIterator dsi2 = d2.getIterator();
		while (!dsi1.atEnd()) {
			DataSetElement dse1 = dsi1.getElement();
			DataSetElement dse2 = dsi2.getElement();
			index = dsi1.getIndex();
			if (_filter.isAcceptable(dse1) && _filter.isAcceptable(dse2)) {
				double y1 = dse1.getY();
				double y2 = dse2.getY();
				if (operationId == ADD)
					y[index] = y1 + y2;
				else if (operationId == SUB)
					y[index] = y1 - y2;
				else if (operationId == MUL)
					y[index] = y1 * y2;
				else if (operationId == DIV)
					y[index] = y1 / y2;
				else
					throw new IllegalArgumentException(
							"Unknown operation on time series, Operation Id: "
									+ operationId);
				if (flags != null) {
					if (d1.isFlagged()) {
						flags[index] = dse1.getFlag();
					} else if (d2.isFlagged()) {
						flags[index] = dse2.getFlag();
					}
				}
			} else {
				y[index] = Constants.MISSING_VALUE;
				if (flags != null){
					flags[index] = 0;
				}	
			}
			dsi1.advance();
			dsi2.advance();
		}

		DataSetAttr attr = createAttributes(d1, d2, operationId);
		RegularTimeSeries result = new RegularTimeSeries(d1.getName()
				+ getOperationName(operationId) + d2.getName(), d1
				.getStartTime(), d1.getTimeInterval(), y, flags, attr);
		return result;
	}

	/**
   *
   */
	public static TimeSeries doBinaryOperation(TimeSeries d1, double d,
			int operationId, boolean reverseArgs) {
		int size = d1.size();
		double[] xarray = null;
		if (d1 instanceof IrregularTimeSeries)
			xarray = new double[size];
		double[] y = new double[size];
		int[] flags = null;
		if (d1.isFlagged()) {
			flags = new int[size];
		}
		DataSetIterator dsi1 = d1.getIterator();
		while (!dsi1.atEnd()) {
			DataSetElement dse1 = dsi1.getElement();
			int index = dsi1.getIndex();
			if (xarray != null)
				xarray[index] = dse1.getX();
			if (_filter.isAcceptable(dse1)) {
				double y1 = 0.0;
				double y2 = 0.0;
				if (reverseArgs) {
					y1 = d;
					y2 = dse1.getY();
				} else {
					y1 = dse1.getY();
					y2 = d;
				}
				if (operationId == ADD)
					y[index] = y1 + y2;
				else if (operationId == SUB)
					y[index] = y1 - y2;
				else if (operationId == MUL)
					y[index] = y1 * y2;
				else if (operationId == DIV)
					y[index] = y1 / y2;
				else
					throw new IllegalArgumentException(
							"Unknown operation on time series"
									+ ", Operation Id: " + operationId);
				if (flags != null) {
					flags[index] = dse1.getFlag();
				}
			} else {
				y[index] = Constants.MISSING_VALUE;
				if (flags != null) {
					flags[index] = 0;
				}
			}
			dsi1.advance();
		}
		TimeSeries result = null;
		DataSetAttr attr = createAttributes(d1, d, operationId);
		if (xarray == null) {
			RegularTimeSeries ts = (RegularTimeSeries) d1;
			result = new RegularTimeSeries(ts.getName()
					+ getOperationName(operationId) + d, ts.getStartTime(), ts
					.getTimeInterval(), y, flags, attr);
		} else {
			result = new IrregularTimeSeries(d1.getName()
					+ getOperationName(operationId) + d, xarray, y, flags, attr);
		}
		return result;
	}

	/**
   *
   */
	public static TimeSeries doBinaryOperation(TimeSeries d1, double[] d,
			int operationId, boolean reverseArgs) {
		int size = d1.size();
		double[] xarray = null;
		if (d1 instanceof IrregularTimeSeries)
			xarray = new double[size];
		double[] y = new double[size];
		DataSetIterator dsi1 = d1.getIterator();
		int counter = 0;
		while (!dsi1.atEnd()) {
			DataSetElement dse1 = dsi1.getElement();
			int index = dsi1.getIndex();
			if (xarray != null)
				xarray[index] = dse1.getX();
			if (_filter.isAcceptable(dse1)) {
				double y1 = 0.0;
				double y2 = 0.0;
				if (reverseArgs) {
					y1 = d[counter % d.length];
					y2 = dse1.getY();
				} else {
					y1 = dse1.getY();
					y2 = d[counter % d.length];
				}
				if (operationId == ADD)
					y[index] = y1 + y2;
				else if (operationId == SUB)
					y[index] = y1 - y2;
				else if (operationId == MUL)
					y[index] = y1 * y2;
				else if (operationId == DIV)
					y[index] = y1 / y2;
				else
					throw new IllegalArgumentException(
							"Unknown operation on time series"
									+ ", Operation Id: " + operationId);
			} else {
				y[index] = Constants.MISSING_VALUE;
			}
			dsi1.advance();
			counter++;
		}
		TimeSeries result = null;
		DataSetAttr attr = createAttributes(d1, d, operationId);
		String str = "[";
		for (int i = 0; i < Math.min(d.length, 3); i++)
			str = str + d[i] + ",";
		str = str + "...]";
		if (xarray == null) {
			RegularTimeSeries ts = (RegularTimeSeries) d1;
			result = new RegularTimeSeries(ts.getName()
					+ getOperationName(operationId) + d, ts.getStartTime(), ts
					.getTimeInterval(), y, null, attr);
		} else {
			result = new IrregularTimeSeries(d1.getName()
					+ getOperationName(operationId) + d, xarray, y, null, attr);
		}
		return result;
	}

	/**
   *
   */
	public static TimeSeries doBinaryOperation(TimeSeries d1, TimeSeries d2,
			int operationId) {
		if (d1 instanceof RegularTimeSeries && d2 instanceof RegularTimeSeries) {
			return doBinaryOperation((RegularTimeSeries) d1,
					(RegularTimeSeries) d2, operationId);
		}

		DataSetIterator iterator = new MultiIterator(
				new TimeSeries[] { d1, d2 }, null);
		int sz = Math.max(d1.size(), d2.size());
		double[] xarray = new double[sz];
		double[] yarray = new double[sz];
		int index = 0;
		double val;
		while (!iterator.atEnd()) {
			DataSetElement dse = iterator.getElement();
			xarray[index] = dse.getX(0);
			double y1 = dse.getX(1);
			double y2 = dse.getX(2);
			if (y1 == Float.NaN || y2 == Float.NaN) {
				yarray[index++] = Constants.MISSING_VALUE;
			} else {
				switch (operationId) {
				case ADD:
					val = y1 + y2;
					break;
				case MUL:
					val = y1 * y2;
					break;
				case SUB:
					val = y1 / y2;
					break;
				case DIV:
					val = y1 - y2;
					break;
				default:
					throw new IllegalArgumentException(
							"Unknown operation on time series, Operation Id: "
									+ operationId);
				}
				yarray[index++] = val;
			}
			iterator.advance();
		}
		DataSetAttr attr = createAttributes(d1, d2, operationId);
		TimeSeries result = new IrregularTimeSeries(d1.getName()
				+ getOperationName(operationId) + d2.getName(), xarray, yarray,
				null, attr);
		return result;
	}

	/**
   *
   */
	public static TimeSeries doUnitConversion(TimeSeries d1, String toUnit) {
		DataSetAttr attr1 = d1.getAttributes();
		String fromUnit = attr1.getYUnits();
		double ratio = Units.getConversionRatio(fromUnit, toUnit);
		TimeSeries d2 = doBinaryOperation(d1, ratio, MUL, false);
		// set units: to be done later
		DataSetAttr attr2 = new DataSetAttr(attr1.getType(), attr1.getXUnits(),
				toUnit, attr1.getXType(), attr1.getYType());
		return d2;

	}

	/**
   *
   */
	private static int nLastValues = 25;

	/**
	 * sets the number of last values in a period to be used for last period
	 * operations
	 */
	public static void setNumberOfLastValues(int n) {
		nLastValues = n;
	}

	/**
   *
   */
	public static RegularTimeSeries doPeriodOperation(RegularTimeSeries ds,
			TimeInterval ti, int periodOperationId) {

		TimeInterval dsti = ds.getTimeInterval();
		int maxvals = getMaximumNumberOfValues(dsti, ti);
		double[] yvals = new double[maxvals];
		double[] yArray = new double[ds.size()];
		DataSetIterator dsi = ds.getIterator();
		DataSetElement dse = dsi.getElement();
		Time nstime = ds.getStartTime().create(ds.getStartTime()).ceiling(ti);
		int yIndex = -1, vIndex = 0;
		Time stime = nstime.create(ds.getStartTime());
		int nvals = 0;
		// System.out.println("maxvals: " + maxvals);
		while (!dsi.atEnd()) {
			vIndex = 0;
			dse = dsi.getElement();
			stime.incrementBy(dsti, nvals);
			String str_intvl = ti.toString();
			if (str_intvl.equals("1HOUR") || str_intvl.equals("1DAY")
					|| str_intvl.equals("1MON")) {
				nvals = getNumberOfValues(stime, dsti, ti) + 1;
			} else {
				nvals = ti.__div__(dsti);
			}
			// collect values for the give intervals
			for (int i = 0; i < nvals; i++) {
				dse = dsi.getElement();
				// System.out.println("nvals: "+nvals);
				// System.out.println("dse: "+dse);
				if (_filter.isAcceptable(dse)) {
					yvals[vIndex] = dse.getY();
					vIndex++;
				}
				dsi.advance();
				if (dsi.atEnd())
					break;
			}
			// System.out.println("vIndex: "+vIndex);
			// do required operations on values and store in new array.
			yIndex++;
			if (vIndex > (_operationViableThreshold * maxvals)) {
				yArray[yIndex] = doOperation(yvals, vIndex, periodOperationId);
			} else {
				yArray[yIndex] = Constants.MISSING_VALUE;
			}
		}
		// resize arrays
		double[] tmpArray = new double[yIndex + 1];
		System.arraycopy(yArray, 0, tmpArray, 0, tmpArray.length);
		yArray = tmpArray;
		// create start time again...
		nstime = ds.getStartTime().create(ds.getStartTime()).ceiling(ti);
		// create end time
		Time netime = nstime.create(nstime);
		netime.incrementBy(ti, yIndex);
		// create data set
		String dsname = ds.getName() + getOperationName(periodOperationId)
				+ ti.getIntervalAsString();
		DataSetAttr attr = ds.getAttributes();
		if (attr != null) {
			attr = new DataSetAttr(attr.getGroupName(), attr.getLocationName()
					+ getOperationName(periodOperationId), attr.getTypeName(),
					attr.getSourceName(), attr.getType(), attr.getXUnits(),
					attr.getYUnits(), attr.getXType(), "PER-AVER");
		}
		return new RegularTimeSeries(dsname, nstime, ti, yArray, null, attr);
	}

	/**
	 * Creates an irregular time series for the periods min/max and the time
	 * when it occurs is stored along with the value Note: The logic gets the
	 * first time the value occurs within the period rather than the last time
	 * the value occurs.
	 */
	public static IrregularTimeSeries getPeriodMinMax(RegularTimeSeries ds,
			TimeInterval ti, int periodOperationId) {
		if (periodOperationId != PERIOD_MIN && periodOperationId != PERIOD_MAX) {
			throw new IllegalArgumentException("Only period min or max allowed");
		}
		TimeInterval dsti = ds.getTimeInterval();
		int maxvals = getMaximumNumberOfValues(dsti, ti);
		double[] yArray = new double[ds.size()];
		double[] xArray = new double[ds.size()];
		DataSetIterator dsi = ds.getIterator();
		DataSetElement dse = dsi.getElement();
		Time nstime = ds.getStartTime().create(ds.getStartTime()).ceiling(ti);
		int yIndex = -1, vIndex = 0;
		Time stime = nstime.create(ds.getStartTime());
		int nvals = 0;
		// System.out.println("maxvals: " + maxvals);
		while (!dsi.atEnd()) {
			vIndex = 0;
			dse = dsi.getElement();
			stime.incrementBy(dsti, nvals);
			String str_intvl = ti.toString();
			if (str_intvl.equals("1HOUR") || str_intvl.equals("1DAY")
					|| str_intvl.equals("1MON")) {
				nvals = getNumberOfValues(stime, dsti, ti) + 1;
			} else {
				nvals = ti.__div__(dsti);
			}
			// collect values for the give intervals
			double xval = dse.getX();
			double yval = Float.NaN;
			for (int i = 0; i < nvals; i++) {
				dse = dsi.getElement();
				if (_filter.isAcceptable(dse)) {
					if (Double.isNaN(yval)) {
						xval = dse.getX();
						yval = dse.getY();
					} else {
						if (periodOperationId == PERIOD_MAX) {
							if (yval < dse.getY()) {
								xval = dse.getX();
								yval = dse.getY();
							}
						} else if (periodOperationId == PERIOD_MIN) {
							if (yval > dse.getY()) {
								xval = dse.getX();
								yval = dse.getY();
							}
						}
					}
					vIndex++;
				}
				dsi.advance();
				if (dsi.atEnd())
					break;
			}
			if (Double.isNaN(yval)) {
				yval = Constants.MISSING_VALUE;
			}
			yIndex++;
			yArray[yIndex] = yval;
			xArray[yIndex] = xval;
		}
		// resize arrays
		double[] tmpArray = new double[yIndex + 1];
		System.arraycopy(yArray, 0, tmpArray, 0, tmpArray.length);
		yArray = tmpArray;
		tmpArray = new double[yIndex + 1];
		System.arraycopy(xArray, 0, tmpArray, 0, tmpArray.length);
		xArray = tmpArray;
		// create start time again...
		nstime = ds.getStartTime().create(ds.getStartTime()).ceiling(ti);
		// create end time
		Time netime = nstime.create(nstime);
		netime.incrementBy(ti, yIndex);
		// create data set
		String dsname = ds.getName() + getOperationName(periodOperationId)
				+ ti.getIntervalAsString();
		DataSetAttr attr = ds.getAttributes();
		if (attr != null) {
			attr = new DataSetAttr(attr.getGroupName(), attr.getLocationName()
					+ getOperationName(periodOperationId), attr.getTypeName(),
					attr.getSourceName(), attr.getType(), attr.getXUnits(),
					attr.getYUnits(), attr.getXType(), "INST-VAL");
		}
		return new IrregularTimeSeries(dsname, xArray, yArray, null, attr);
	}

	/**
   *
   */
	private static float _operationViableThreshold = 0.25f;

	/**
   *
   */
	private static double doOperation(double[] yvals, int nvals,
			int periodOperationId) {
		if (periodOperationId == PERIOD_AVERAGE) {
			return doPeriodAverage(yvals, nvals);
		} else if (periodOperationId == PERIOD_MIN) {
			return doPeriodMin(yvals, nvals);
		} else if (periodOperationId == PERIOD_MAX) {
			return doPeriodMax(yvals, nvals);
		} else if (periodOperationId == PERIOD_LAST_AVERAGE) {
			return doPeriodLastAverage(yvals, nvals);
		} else if (periodOperationId == PERIOD_LAST_MAX) {
			return doPeriodLastMax(yvals, nvals);
		} else if (periodOperationId == PERIOD_LAST_MIN) {
			return doPeriodLastMin(yvals, nvals);
		} else {
			throw new IllegalArgumentException("Unknown period Operation id : "
					+ periodOperationId);
		}

	}

	/**
   *
   */
	private static double doPeriodAverage(double[] yvals, int nvals) {
		double sum = 0.0;
		for (int i = 0; i < nvals; i++) {
			sum += yvals[i];
		}
		return sum / nvals;
	}

	/**
   *
   */
	private static double doPeriodMin(double[] yvals, int nvals) {
		if (nvals <= 0)
			return 0;
		double min = yvals[0];
		for (int i = 1; i < nvals; i++) {
			min = Math.min(min, yvals[i]);
		}
		return min;
	}

	/**
   *
   */
	private static double doPeriodMax(double[] yvals, int nvals) {
		if (nvals <= 0)
			return 0;
		double max = yvals[0];
		for (int i = 1; i < nvals; i++) {
			max = Math.max(max, yvals[i]);
		}
		return max;
	}

	/**
   *
   */
	private static double doPeriodLastAverage(double[] yvals, int nvals) {
		if (nvals < nLastValues)
			return 0;
		double sum = 0;
		for (int i = nvals - nLastValues; i < nvals; i++) {
			sum += yvals[i];
		}
		return sum / nLastValues;
	}

	/**
   *
   */
	private static double doPeriodLastMax(double[] yvals, int nvals) {
		if (nvals <= nLastValues)
			return 0;
		double max = yvals[nvals - nLastValues];
		for (int i = nvals - nLastValues + 1; i < nvals; i++) {
			max = Math.max(max, yvals[i]);
		}
		return max;
	}

	/**
   *
   */
	private static double doPeriodLastMin(double[] yvals, int nvals) {
		if (nvals <= nLastValues)
			return 0;
		double min = yvals[nvals - nLastValues];
		for (int i = nvals - nLastValues + 1; i < nvals; i++) {
			min = Math.min(min, yvals[i]);
		}
		return min;
	}

	/**
   *
   */
	private static int getNumberOfValues(Time tm, TimeInterval oti,
			TimeInterval nti) {
		Time ntm = tm.ceiling(nti);
		return (int) tm.getExactNumberOfIntervalsTo(ntm, oti);
	}

	/**
   *
   */
	private static int getMaximumNumberOfValues(TimeInterval oti,
			TimeInterval nti) {
		Time timeproto = TimeFactory.getInstance().getTimeInstance();
		Time atime = timeproto.create(0);
		Time etime = timeproto.create(0);
		etime.incrementBy(nti, 2);
		return (int) atime.getExactNumberOfIntervalsTo(etime, oti);
	}

	/**
   *
   */
	public static RegularTimeSeries merge(RegularTimeSeries d1,
			RegularTimeSeries d2, ElementFilter filter) {
		if (filter == null)
			filter = Constants.DEFAULT_FLAG_FILTER;
		RegularTimeSeries[] d12 = createMergeCompatible(d1, d2);
		d1 = d12[0];
		d2 = d12[1];
		TimeInterval ti = d1.getTimeInterval();
		int nvals = d1.size();
		double[] yArray = new double[nvals];
		DataSetIterator dsi = new MultiIterator(d12, filter);
		Time stime = d1.getStartTime().create(
				Math.round(dsi.getElement().getX()));
		while (!dsi.atEnd()) {
			DataSetElement dse = dsi.getElement();
			int index = dsi.getIndex();
			double y1 = dse.getX(1);
			if (y1 == Constants.MISSING_VALUE || y1 == Constants.MISSING
					|| y1 == Constants.MISSING_RECORD
					|| Double.doubleToLongBits(y1) == 0x7ff8000000000000L) {
				double y2 = dse.getX(2);
				if (y2 == Constants.MISSING_VALUE || y2 == Constants.MISSING
						|| y2 == Constants.MISSING_RECORD
						|| Double.doubleToLongBits(y2) == 0x7ff8000000000000L) {
					yArray[index] = Constants.MISSING_VALUE;
				} else {
					yArray[index] = y2;
				}
			} else {
				yArray[index] = y1;
			}
			dsi.advance();
		}
		DataSetAttr attr = createAttributes(d1, d2);
		RegularTimeSeries result = new RegularTimeSeries(d1.getName() + "merge"
				+ d2.getName(), stime, ti, yArray, null, attr);
		return result;
	}

	/**
	 * does not bother about their being of different types or being of
	 * different units
	 */
	static RegularTimeSeries[] createMergeCompatible(RegularTimeSeries d1,
			RegularTimeSeries d2) {
		DataSetAttr attr1 = d1.getAttributes();
		DataSetAttr attr2 = d2.getAttributes();
		if (attr1 == null || attr2 == null) {
		} else {
		}
		TimeInterval ti1 = d1.getTimeInterval();
		TimeInterval ti2 = d2.getTimeInterval();
		TimeInterval ti;
		if (DUMB_PATCH) {
			if (ti1.compare(ti2) < 0)
				ti = ti2;
			else
				ti = ti1;
			if (ti1.compare(ti) != 0)
				d1 = doPeriodOperation(d1, ti, PERIOD_AVERAGE);
			if (ti2.compare(ti) != 0)
				d2 = doPeriodOperation(d2, ti, PERIOD_AVERAGE);
		} else {
			if (ti1.compare(ti2) != 0)
				throw new IllegalArgumentException(
						"Incompatible time intervals: " + ti1 + " & " + ti2);
		}
		return new RegularTimeSeries[] { d1, d2 };
	}

	/**
   *
   */
	public static TimeSeries merge(TimeSeries d1, TimeSeries d2) {
		boolean bothRTS = false;
		if (d1 instanceof RegularTimeSeries && d2 instanceof RegularTimeSeries) {
			RegularTimeSeries[] d12 = createMergeCompatible(
					(RegularTimeSeries) d1, (RegularTimeSeries) d2);
			d1 = d12[0];
			d2 = d12[1];
			// return merge((RegularTimeSeries) d1, (RegularTimeSeries) d2);
			bothRTS = true;
		}
		// if not do this
		TimeSeries[] d12 = new TimeSeries[] { d1, d2 };
		// set filter for this operation
		DataSetIterator dsi = new MultiIterator(d12,
				Constants.DEFAULT_FLAG_FILTER);
		double[] yArray = new double[d1.size() + d2.size()]; // worst case
		double[] xArray = null;
		if (!bothRTS)
			xArray = new double[d1.size() + d2.size()]; // worst case
		// set array to missing
		for (int i = 0; i < yArray.length; i++)
			yArray[i] = Constants.MISSING_VALUE;
		int count = 0;
		//
		while (!dsi.atEnd()) {
			DataSetElement dse = dsi.getElement();
			int index = dsi.getIndex();
			double y1 = dse.getX(1);
			if (y1 == Constants.MISSING_VALUE || y1 == Constants.MISSING
					|| y1 == Constants.MISSING_RECORD
					|| Double.doubleToLongBits(y1) == 0x7ff8000000000000L) {
				double y2 = dse.getX(2);
				if (y2 == Constants.MISSING_VALUE || y2 == Constants.MISSING
						|| y2 == Constants.MISSING_RECORD
						|| Double.doubleToLongBits(y2) == 0x7ff8000000000000L) {
					yArray[index] = Constants.MISSING_VALUE;
				} else {
					yArray[index] = y2;
				}
			} else {
				yArray[index] = y1;
			}
			xArray[index] = dse.getX();
			dsi.advance();
			count++;
		}
		// resize the arrays
		double[] tmpArray = null;
		if (!bothRTS) {
			tmpArray = new double[count];
			System.arraycopy(xArray, 0, tmpArray, 0, tmpArray.length);
			xArray = tmpArray;
		}
		tmpArray = new double[count];
		System.arraycopy(yArray, 0, tmpArray, 0, tmpArray.length);
		yArray = tmpArray;
		// get a wide time window
		Time stime1 = d1.getStartTime().create(d1.getStartTime());
		Time etime1 = stime1.create(d1.getEndTime());
		Time stime2 = d2.getStartTime().create(d2.getStartTime());
		Time etime2 = stime2.create(d2.getEndTime());
		Time stime = null, etime = null;
		if (stime1.compare(stime2) > 0)
			stime = stime2;
		else
			stime = stime1;
		DataSetAttr attr = createAttributes(d1, d2);
		if (!bothRTS) {
			IrregularTimeSeries result = new IrregularTimeSeries(d1.getName()
					+ "merge" + d2.getName(), xArray, yArray, null, attr);
			return result;
		} else {
			TimeInterval ti = ((RegularTimeSeries) d1).getTimeInterval();
			RegularTimeSeries result = new RegularTimeSeries(d1.getName()
					+ "merge" + d2.getName(), stime, ti, yArray, null, attr);
			return result;
		}
	}

	public static RegularTimeSeries createShifted(RegularTimeSeries ts,
			TimeInterval ti) {
		double[] y = SetUtils.createYArray(ts);
		int[] flags = null;
		if (ts.isFlagged()) {
			flags = SetUtils.createFlagArray(ts);
		}
		Time stime = ts.getStartTime();
		stime = stime.create(stime);
		stime.incrementBy(ti);
		return new RegularTimeSeries(ts.getName(), stime, ts.getTimeInterval(),
				y, flags, ts.getAttributes());

	}

	/**
   *
   */
	public static RegularTimeSeries createShifted(RegularTimeSeries ts,
			int shift) {
		TimeInterval ti = ts.getTimeInterval().__mul__(shift);
		return createShifted(ts, ti);
	}

	/**
	 * returns the forward difference ts(t+1)-ts(t)
	 */
	public static RegularTimeSeries forwardDifference(RegularTimeSeries ts) {
		RegularTimeSeries ts1 = createShifted(ts, 1);
		return ts.__sub__(ts1);
	}

	/**
   *
   */
	public static IrregularTimeSeries createShifted(IrregularTimeSeries ts,
			TimeInterval ti) {
		int ncount = ts.size();
		double[] x = new double[ncount];
		double[] y = new double[ncount];
		int[] flags = null;
		if (ts.isFlagged()) {
			flags = new int[ncount];
		}
		DataSetElement dse;
		DataSetIterator dsi = ts.getIterator();
		TimeFactory tf = TimeFactory.getInstance();
		for (int i = 0; i < ncount; i++) {
			dsi.positionAtIndex(i);
			dse = dsi.getElement();
			x[i] = dse.getX();
			y[i] = dse.getY();
			Time tm = tf.createTime(Math.round(x[i]));
			tm.incrementBy(ti);
			x[i] = tm.getTimeInMinutes();
			if (ts.isFlagged()) {
				flags[i] = dse.getFlag();
			}
		}
		return new IrregularTimeSeries(ts.getName(), x, y, flags, ts
				.getAttributes());
	}

	/**
   *
   */
	public static TimeSeries sin(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.sin(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries cos(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.cos(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries tan(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.tan(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries asin(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.asin(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries acos(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.acos(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries atan(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.atan(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries log(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.log(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries exp(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.exp(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries sqrt(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.sqrt(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries ceil(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.ceil(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static TimeSeries floor(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.floor(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
	 * returns the closest integer to the argument.
	 * 
	 */
	public static TimeSeries rint(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.rint(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
	 * Returns of value of the first argument raised to the power of the second
	 * argument.
	 */
	public static TimeSeries pow(TimeSeries ts, double power) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.pow(y, power));
			iter.putElement(e);
		}
		return nts;
	}

	/**
	 * Returns of value of the first argument raised to the power of the second
	 * argument.
	 */
	public static TimeSeries abs(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.abs(y));
			iter.putElement(e);
		}
		return nts;
	}

	/**
	 * Returns the closest <code>int</code> to the argument.
	 * <p>
	 */
	public static TimeSeries round(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			double y = e.getY();
			e.setY(Math.round(y));
			iter.putElement(e);
		}
		return nts;
	}

	private static Random randomNumberGenerator;

	/**
	 * 
	 * Returns a random time series between <code>0.0</code> and
	 * <code>1.0</code> with the given time series as a template
	 */
	public static synchronized TimeSeries random(TimeSeries ts) {
		if (randomNumberGenerator == null)
			randomNumberGenerator = new Random();
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		for (DataSetIterator iter = nts.getIterator(); !iter.atEnd(); iter
				.advance()) {
			DataSetElement e = iter.getElement();
			e.setY(Math.floor(randomNumberGenerator.nextDouble()));
			iter.putElement(e);
		}
		return nts;
	}

	/**
   *
   */
	public static double sum(TimeSeries ts) {
		double sumy = 0.0;
		DataSetIterator iter;
		for (iter = ts.getIterator(); !iter.atEnd(); iter.advance()) {
			DataSetElement e = iter.getElement();
			if (_filter.isAcceptable(e))
				sumy += e.getY();
		}
		return sumy;
	}

	/**
	 * replace un acceptable values with linearly interpolated values if
	 * possible. No values beyond last and before first acceptable values are
	 * estimated.
	 */
	public static TimeSeries estlin(TimeSeries ts) {
		TimeSeries nts = ts.createSlice(ts.getTimeWindow());
		DataSetIterator iter1 = nts.getIterator();
		DataSetIterator iter2 = nts.getIterator();
		// get first good value
		while (!iter1.atEnd() && !_filter.isAcceptable(iter1.getElement()))
			iter1.advance();
		while (!iter2.atEnd() && !_filter.isAcceptable(iter2.getElement()))
			iter2.advance();
		// if no good values found return null;
		if (iter1.atEnd())
			return null;
		DataSetElement lastGoodValue = iter1.getElement().createClone();
		while (!iter1.atEnd()) {
			// get to first unacceptable value
			while (!iter1.atEnd() && _filter.isAcceptable(iter1.getElement())) {
				iter1.advance();
				iter2.advance();
			}
			if (iter1.atEnd())
				break;
			// get to next good value
			while (!iter1.atEnd() && !_filter.isAcceptable(iter1.getElement())) {
				iter1.advance();
			}
			if (iter1.atEnd())
				break;
			// rewind iter2 by one to get last good value and advance to reset
			iter2.retreat();
			DataSetElement e1 = iter2.getElement().createClone();
			iter2.advance();
			DataSetElement e2 = iter1.getElement().createClone();
			double x1 = e1.getX(), y1 = e1.getY();
			double x2 = e2.getX(), y2 = e2.getY();
			// put interpolated values into time series
			while (iter2.getIndex() != iter1.getIndex()) {
				double x = iter2.getElement().getX();
				double iy = (y2 - y1) / (x2 - x1) * (x - x1) + y1;
				iter2.getElement().setY(iy);
				iter2.putElement(iter2.getElement());
				iter2.advance();
			}
		}
		return nts;
	}

	/**
   *
   */
	public final static int SNAP_NEAREST = 1, SNAP_BACKWARD = 2,
			SNAP_FORWARD = 3;

	/**
	 * snaps values in a regular time series to nearest time value. If more than
	 * values snap to the same value then the value with the latest time stamp
	 * is set to that value
	 */
	public static RegularTimeSeries snap(IrregularTimeSeries its,
			TimeInterval ti, int SNAP_TYPE) {
		boolean snapToNearest = false, snapBackward = false, snapForward = false;
		if (SNAP_TYPE == SNAP_NEAREST) {
			snapToNearest = true;
		} else if (SNAP_TYPE == SNAP_BACKWARD) {
			snapBackward = true;
		} else if (SNAP_TYPE == SNAP_FORWARD) {
			snapForward = true;
		} else {
			snapToNearest = true;
		}
		TimeWindow tw = its.getTimeWindow();
		// get starting and ending times
		Time stime = tw.getStartTime();
		stime = stime.create(stime);
		if (snapToNearest) {
			stime = stime.round(ti);
		} else if (snapBackward) {
			stime = stime.floor(ti);
		} else if (snapForward) {
			stime = stime.ceiling(ti);
		}
		Time etime = tw.getEndTime();
		etime = etime.create(etime);
		if (snapToNearest) {
			etime = etime.round(ti);
		} else if (snapBackward) {
			etime = etime.floor(ti);
		} else if (snapForward) {
			etime = etime.ceiling(ti);
		}
		// get number of values
		int nvals = (int) stime.getExactNumberOfIntervalsTo(etime, ti) + 1;
		// create an array of missing values
		double[] y = new double[nvals];
		for (int i = 0; i < y.length; i++) {
			y[i] = Constants.MISSING_VALUE;
		}
		int[] flags = null;
		if (its.isFlagged()) {
			flags = new int[nvals];
			for (int i = 0; i < y.length; i++) {
				flags[i] = 0;
			}
		}
		// set the values
		DataSetIterator iter = its.getIterator();
		for (; !iter.atEnd(); iter.advance()) {
			Time tm = TimeFactory.getInstance().createTime(
					Math.round(iter.getElement().getX()));
			if (snapToNearest) {
				tm = tm.round(ti);
			} else if (snapBackward) {
				tm = tm.floor(ti);
			} else if (snapForward) {
				tm = tm.ceiling(ti);
			}
			int index = (int) stime.getExactNumberOfIntervalsTo(tm, ti);
			y[index] = iter.getElement().getY();
			if (its.isFlagged()) {
				flags[index] = iter.getElement().getFlag();
			}
		}
		// create the attributes
		DataSetAttr oa = its.getAttributes();
		DataSetAttr attr = new DataSetAttr(oa.getGroupName(), oa
				.getLocationName(), oa.getTypeName(), oa.getSourceName(),
				DataType.REGULAR_TIME_SERIES, oa.getXUnits(), oa.getYUnits(),
				oa.getXType(), oa.getYType());
		// create a regular time series and return it
		return new RegularTimeSeries(its.getName(), stime.toString(), ti
				.toString(), y, flags, attr);
	}

	/**
	 * Creates a regular time series sample every ti (time interval) from the
	 * provided time series, ts using last value or interpolation at every point
	 * linearly interpolatee value between that point and the next point.
	 * <p>
	 * Flags are inherited for last value in the case of LAST_VAL
	 * <p>
	 * If the next point does not exist (end of series) then the last value is
	 * used.
	 * 
	 * @param ts
	 * @param ti
	 * @param sampleType
	 *            either TimeSeriesMath.LINEAR or TimeSeriesMath.LAST_VAL
	 * @return a regular time series with time interval of ti
	 */
	public static RegularTimeSeries sample(TimeSeries ts, TimeInterval ti,
			int sampleType) {
		TimeWindow tw = ts.getTimeWindow();
		// get starting and ending times
		Time stime = tw.getStartTime();
		stime = stime.create(stime);
		stime.floor(ti);
		Time etime = tw.getEndTime();
		etime = etime.create(etime);
		etime = etime.ceiling(ti);
		// create the attributes
		DataSetAttr oa = ts.getAttributes();
		DataSetAttr attr = new DataSetAttr(oa.getGroupName(), oa
				.getLocationName(), oa.getTypeName(), oa.getSourceName(),
				DataType.REGULAR_TIME_SERIES, oa.getXUnits(), oa.getYUnits(),
				oa.getXType(), oa.getYType());
		// get number of values
		int nvals = (int) stime.getExactNumberOfIntervalsTo(etime, ti) + 1;
		int[] flags = null;
		if (ts.isFlagged()) {
			flags = new int[nvals];
		}
		// create a regular time series
		RegularTimeSeries rts = new RegularTimeSeries(ts.getName(), stime
				.toString(), ti.toString(), new double[nvals], flags, attr);

		DataSetIterator iter = rts.getIterator();
		TimeSeriesIterator iter2 = (TimeSeriesIterator) ts.getIterator();
		TimeFactory TF = TimeFactory.getInstance();
		for (; !iter.atEnd(); iter.advance()) {
			DataSetElement element = iter.getElement();
			iter2.positionAtTime(TF.createTime(element.getXString()));
			TimeElement elementAt = (TimeElement) iter2.getElement();
			if (sampleType == LAST_VAL) {
				element.setY(elementAt.getY());
				if (ts.isFlagged()) {
					element.setFlag(elementAt.getFlag());
				}
			} else if (sampleType == LINEAR) {
				if (_filter.isAcceptable(elementAt)) {
					elementAt = (TimeElement) elementAt.createClone();
					TimeElement nextElement = (TimeElement) iter2.getElement();
					if (_filter.isAcceptable(nextElement)) {
						element.setY(calculateLinearlyInterpolatedValue(
								element, elementAt, nextElement));
						element.setFlag(elementAt.getFlag());
					}
				} else {
					element.setY(Constants.MISSING);
					element.setFlag(0);
				}
			} else {
				element.setY(Constants.MISSING);
			}
			iter.putElement(element);
		}
		return rts;
	}

	private static double calculateLinearlyInterpolatedValue(
			DataSetElement element, TimeElement elementAt,
			TimeElement nextElement) {
		double x1 = elementAt.getX();
		double y1 = elementAt.getY();
		double x2 = nextElement.getX();
		double y2 = nextElement.getY();
		double x = element.getX();
		if (x2 == x1) {
			return y1;
		}
		return (y2 - y1) / (x2 - x1) * (x - x1) + y1;
	}

	/**
     *
     */
	public static DefaultDataSet mate(TimeSeries ts1, TimeSeries ts2,
			boolean sortByTime) {
		MultiIterator mi = new MultiIterator(new TimeSeries[] { ts1, ts2 },
				_filter);
		int maxsize = Math.max(ts1.size(), ts2.size());
		double[] x = new double[maxsize];
		double[] y = new double[maxsize];
		int count = 0;
		for (; !mi.atEnd(); mi.advance()) {
			DataSetElement e = mi.getElement();
			if (Double.doubleToLongBits(e.getX(1)) != 0x7ff8000000000000L
					&& Double.doubleToLongBits(e.getX(2)) != 0x7ff8000000000000L) {
				x[count] = e.getX(1);
				y[count] = e.getX(2);
				count++;
			}
		}
		if (count < maxsize) {
			double[] ta = new double[count];
			System.arraycopy(x, 0, ta, 0, ta.length);
			x = ta;
			ta = new double[count];
			System.arraycopy(y, 0, ta, 0, ta.length);
			y = ta;
		}
		return new DefaultDataSet("mated " + ts1.getName() + " & "
				+ ts2.getName(), x, y);
	}

	/**
	 * auto_corr(rts) =
	 * sigma(i=0,N-1)(x(i)-mx)(x(i+k)-mx)/sigma(i=0,N-1)(x(i)-mx)
	 * 
	 */
	public static DefaultDataSet getAutoCorrelation(RegularTimeSeries rts,
			int maxLag) {
		double mx = Stats.avg(rts);
		double[] x = new double[maxLag];
		double[] y = new double[maxLag];
		for (int i = 0; i < maxLag; i++) {
			x[i] = i + 1;
			y[i] = calculateAutoCorrelation(rts, i + 1, mx);
		}
		return new DefaultDataSet("auto-correlation of " + rts.getName(), x, y);
	}

	/**
   *
   */
	public static double calculateAutoCorrelation(RegularTimeSeries rts,
			int lag, double mean) {
		ElementFilter filter = _filter;
		int count = rts.size();
		if (lag >= count)
			return 0.0;
		double xi, xik;
		double sum1 = 0.0, sum2 = 0.0;
		DataSetIterator iter1 = rts.getIterator();
		DataSetIterator iter2 = rts.getIterator();
		for (int i = 0; i < lag; i++)
			iter2.advance();
		for (; !iter1.atEnd(); iter1.advance(), iter2.advance()) {
			if (iter2.atEnd())
				iter2.resetIterator();
			DataSetElement ei = iter1.getElement();
			DataSetElement eik = iter2.getElement();
			if (filter.isAcceptable(ei) && filter.isAcceptable(eik)) {
				xi = ei.getY();
				xik = eik.getY();
				sum1 += (xi - mean) * (xik - mean);
			} else {
				continue;
			}
			if (filter.isAcceptable(ei)) {
				xi = eik.getY() - mean;
				sum2 += xi * xi;
			} else {
				continue;
			}
		}
		return sum2 == 0 ? 0 : sum1 / sum2;
	}

}

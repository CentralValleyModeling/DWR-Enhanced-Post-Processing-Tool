/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import vista.time.Time;
import vista.time.TimeFactory;

/**
 * A tuple in which the first dimension is time which also indexes the other
 * dimensions
 * 
 * @see DataSet
 * @author Nicky Sandhu (DWR).
 * @version $Id: TimeTuple.java,v 1.1 2003/10/02 20:49:33 redwood Exp $
 */
public class TimeTuple implements DataSetElement {
	private Time _tp = TimeFactory.getInstance().getTimeInstance();

	public TimeTuple() {
	}

	/**
   *
   */
	public TimeTuple(String time, double[] vals, int[] flags) {
		this();
		_x = _tp.create(time).getTimeInMinutes();
		_yvals = vals;
		_flags = flags;
	}

	/**
   *
   */
	public TimeTuple(Time time, double[] vals, int[] flags) {
		this();
		_x = time.getTimeInMinutes();
		_yvals = vals;
		_flags = flags;
	}

	/**
   *
   */
	public TimeTuple(double t, double[] vals, int[] flags) {
		this();
		_x = t;
		_yvals = vals;
		_flags = flags;
	}

	/**
	 * the dimension of the tuple
	 */
	public int getDimension() {
		return _yvals.length + 1;
	}

	/**
   *
   */
	private void checkIndex(int i) {
		if (i < 0 || i >= getDimension())
			throw new IndexOutOfBoundsException("Dimension is "
					+ getDimension() + ", invalid access for index " + i);
	}

	/**
	 * set the i'th dimension value numbered from 0 to n-1.
	 */
	public void setX(int i, double val) {
		checkIndex(i);
		if (i == 0)
			setX(val);
		else
			_yvals[i - 1] = val;
	}

	/**
	 * get the i'th dimension value
	 */
	public double getX(int i) {
		checkIndex(i);
		if (i == 0)
			return getX();
		else
			return getY(i - 1);
	}

	/**
	 * get the i'th dimension value string repr.
	 */
	public String getXString(int i) {
		checkIndex(i);
		if (i == 0)
			return getXString();
		else
			return getYString(i - 1);
	}

	/**
	 * set X
	 */
	public final void setX(double x) {
		_x = x;
	}

	/**
	 * set Y
	 */
	public final void setY(double y) {
		setY(0);
	}

	/**
	 * set Y
	 */
	public final void setY(double[] y) {
		if (_yvals.length != y.length)
			throw new IllegalArgumentException(
					"Incorrect size of y arrray. For dimension "
							+ getDimension() + " use array size = "
							+ _yvals.length);
		_yvals = y;
	}

	/**
	 * set flags
	 */
	public final void setFlags(int[] flags) {
		if (_flags.length != flags.length)
			throw new IllegalArgumentException(
					"Incorrect size of flag arrray. For dimension "
							+ getDimension() + " use array size = "
							+ _flags.length);
		_flags = flags;
	}

	/**
	 * set the i'th dimension value
	 */
	public void setY(int i, double val) {
		setX(i + 1, val);
	}

	/**
	 * set the i'th dimension flag
	 */
	public void setFlag(int i, int flag) {
		checkIndex(i + 1);
		_flags[i] = flag;
	}

	/**
	 * get x
	 */
	public final double getX() {
		return _x;
	}

	/**
	 * return x's representation as string
	 */
	public String getXString() {
		return _tp.create(Math.round(_x)).toString();
	}

	/**
	 * get y
	 */
	public final double getY() {
		return getY(0);
	}

	/**
	 * get y
	 */
	public final double getY(int i) {
		return _yvals[i];
	}

	/**
	 * return y's representation as string
	 */
	public String getYString() {
		return getYString(0);
	}

	/**
	 * return y's representation as string
	 */
	public String getYString(int i) {
		double y = _yvals[i];
		if (y == Constants.MISSING_VALUE || y == Constants.MISSING)
			return "MISSING VALUE";
		else if (y == Constants.MISSING_RECORD)
			return "MISSING RECORD";
		else if (Double.doubleToLongBits(y) == 0x7ff8000000000000L)
			return "----";
		else
			return SetUtils.format(y);
	}

	/**
	 * returns 0
	 */
	public int getFlag() {
		return getFlag(0);
	}

	/**
	 * get flag at the i'th dimension
	 */
	public int getFlag(int i) {
		checkIndex(i + 1);
		return _flags[i];
	}

	/**
	 * returns flag from the i'th dimension as string
	 */
	public String getFlagString(int i) {
		checkIndex(i + 1);
		int flag = getFlag(i);
		return FlagUtils.getQualityFlagName(FlagUtils.getQualityFlag(flag))
				+ " | " + FlagUtils.getLastCheckedBy(flag);
	}

	/**
   *
   */
	public String getFlagString() {
		return getFlagString(0);
	}

	/**
	 * does not do anything.
	 */
	public void setFlag(int flag) {
	}

	/**
	 * copies over the fields from the other element
	 */
	public void copyFrom(DataSetElement dse) {
		if (dse == null)
			return;
		// only two types of elements default and flagged
		TimeTuple e = (TimeTuple) dse;
		_x = e._x;
		_yvals = e._yvals;
		_flags = e._flags;
	}

	/**
	 * creates a copy of itself
	 */
	public DataSetElement createClone() {
		TimeTuple e = new TimeTuple();
		e.copyFrom(this);
		return e;
	}

	/**
	 * string representation
	 */
	public String toString() {
		StringBuffer buf = new StringBuffer(15 * getDimension());
		buf.append("( ").append(getXString());
		for (int i = 0; i < _yvals.length; i++) {
			buf.append(", ");
			buf.append(getYString(i));
		}
		buf.append(" )");
		return buf.toString();
	}

	/**
	 * x value
	 */
	private double _x;
	/**
	 * y value
	 */
	private double[] _yvals;
	private int[] _flags;
}

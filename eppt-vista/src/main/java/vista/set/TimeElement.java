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

import vista.time.Time;
import vista.time.TimeFactory;

/**
 * This class describes the interface for a element which is contained in a
 * DataSet. This would be revised to be more general.
 * 
 * @see DataSet
 * @author Nicky Sandhu (DWR).
 * @version $Id: TimeElement.java,v 1.1 2003/10/02 20:49:33 redwood Exp $
 */
public class TimeElement implements DataSetElement {
	private Time _tp = TimeFactory.getInstance().getTimeInstance();

	public TimeElement() {
	}

	/**
   *
   */
	public TimeElement(String time, double val) {
		this();
		_x = _tp.create(time).getTimeInMinutes();
		_y = val;
	}

	/**
   *
   */
	public TimeElement(Time time, double val) {
		this();
		_x = time.getTimeInMinutes();
		_y = val;
	}

	/**
   *
   */
	public TimeElement(double t, double val) {
		this();
		_x = t;
		_y = val;
	}

	/**
	 * the dimension of the tuple
	 */
	public int getDimension() {
		return 2;
	}

	/**
   *
   */
	void checkIndex(int i) {
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
			setY(val);
	}

	/**
	 * get the i'th dimension value
	 */
	public double getX(int i) {
		checkIndex(i);
		if (i == 0)
			return getX();
		else
			return getY();
	}

	/**
	 * get the i'th dimension value string repr.
	 */
	public String getXString(int i) {
		checkIndex(i);
		if (i == 0)
			return getXString();
		else
			return getYString();
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
		_y = y;
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
		setFlag(flag);
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
		return _y;
	}

	/**
	 * return y's representation as string
	 */
	public String getYString() {
		if (_y == Constants.MISSING_VALUE || _y == Constants.MISSING)
			return "MISSING VALUE";
		else if (_y == Constants.MISSING_RECORD)
			return "MISSING RECORD";
		else if (Double.doubleToLongBits(_y) == 0x7ff8000000000000L)
			return "NaN";
		else
			return SetUtils.format(_y);
	}

	/**
	 * returns 0
	 */
	public int getFlag() {
		return 0;
	}

	/**
   *
   */
	public String getFlagString() {
		return "FLAG UNAVAILABLE";
	}

	/**
	 * get flag at the i'th dimension
	 */
	public int getFlag(int i) {
		checkIndex(i);
		return getFlag();
	}

	/**
	 * returns flag from the i'th dimension as string
	 */
	public String getFlagString(int i) {
		checkIndex(i);
		return getFlagString();
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
		TimeElement e = (TimeElement) dse;
		_x = e._x;
		_y = e._y;
	}

	/**
	 * creates a copy of itself
	 */
	public DataSetElement createClone() {
		TimeElement e = new TimeElement();
		e.copyFrom(this);
		return e;
	}

	/**
	 * string representation
	 */
	public String toString() {
		StringBuffer buf = new StringBuffer(15 * 3);
		buf.append("( ").append(getXString()).append(", ").append(getYString())
				.append(" )");
		return buf.toString();
	}

	/**
   *
   */
	public boolean _lt(TimeElement tse) {
		return (tse.getY() < getY());
	}

	/**
   *
   */
	public boolean _gt(TimeElement tse) {
		return (tse.getY() > getY());
	}

	/**
   *
   */
	public boolean equals(Object obj) {
		return (obj instanceof TimeElement) && (isSameAs((TimeElement) obj));
	}

	/**
   *
   */
	public boolean isSameAs(TimeElement te) {
		return (te.getX() == getX()) && (te.getY() == getY());
	}

	/**
   *
   */
	public boolean __nonzero__() {
		return true;
	}

	/**
	 * x value
	 */
	private double _x;
	/**
	 * y value
	 */
	private double _y;
}

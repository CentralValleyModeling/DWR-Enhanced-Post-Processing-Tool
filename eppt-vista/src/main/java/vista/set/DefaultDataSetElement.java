/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;


/**
 * This class describes the interface for a element which is contained in a
 * DataSet. This would be revised to be more general.
 * 
 * @see DataSet
 * @author Nicky Sandhu (DWR).
 * @version $Id: DefaultDataSetElement.java,v 1.1 2003/10/02 20:49:22 redwood
 *          Exp $
 */
public class DefaultDataSetElement implements DataSetElement {
	public DefaultDataSetElement() {
	}

	public DefaultDataSetElement(double x, double y) {
		this();
		setX(x);
		setY(y);
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
	 * get x
	 */
	public final double getX() {
		return _x;
	}

	/**
	 * return x's representation as string
	 */
	public String getXString() {
		return SetUtils.format(_x);
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
		return SetUtils.format(_y);
	}

	/**
	 * returns 0
	 */
	public int getFlag() {
		return 0;
	}

	/**
	 * get flag at the i'th dimension
	 */
	public int getFlag(int i) {
		checkIndex(i + 1);
		return getFlag();
	}

	/**
	 * returns flag from the i'th dimension as string
	 */
	public String getFlagString(int i) {
		checkIndex(i + 1);
		return getFlagString();
	}

	/**
   *
   */
	public String getFlagString() {
		return "FLAG UNAVAILABLE";
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
		DefaultDataSetElement e = (DefaultDataSetElement) dse;
		_x = e._x;
		_y = e._y;
	}

	/**
	 * creates a copy of itself
	 */
	public DataSetElement createClone() {
		DefaultDataSetElement e = new DefaultDataSetElement();
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
	 * x value
	 */
	private double _x;
	/**
	 * y value
	 */
	private double _y;
}
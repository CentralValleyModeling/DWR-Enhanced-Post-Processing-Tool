/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * This class describes the interface for a element which is contained in a
 * DataSet. The reason for defining it as an interface is to save memory when
 * flags are not needed.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: DataSetElement.java,v 1.1 2003/10/02 20:49:21 redwood Exp $
 * @see DataSet
 */
public interface DataSetElement extends java.io.Serializable
{
	/**
	 * get x
	 */
	double getX();

	/**
	 * set X
	 */
	void setX(double x);

	/**
	 * return x's representation as string
	 */
	String getXString();

	/**
	 * get y
	 */
	double getY();

	/**
	 * set Y
	 */
	void setY(double y);

	/**
	 * return y's representation as string
	 */
	String getYString();

	/**
	 * the dimension of the tuple
	 */
	int getDimension();

	/**
	 * set the i'th dimension value
	 */
	void setX(int i, double val);

	/**
	 * get the i'th dimension value
	 */
	double getX(int i);

	/**
	 * set the i'th dimension value
	 */
	void setY(int i, double val);

	/**
	 * set the i'th dimension flag
	 */
	void setFlag(int i, int flag);

	/**
	 * get the i'th dimension value string repr.
	 */
	String getXString(int i);

	/**
	 * get flag
	 */
	int getFlag();

	/**
	 * set flag
	 */
	void setFlag(int flag);

	/**
	 * get flag at the i'th dimension
	 */
	int getFlag(int i);

	/**
	 * returns flag as string
	 */
	String getFlagString();

	/**
	 * returns flag from the i'th dimension as string
	 */
	String getFlagString(int i);

	/**
	 * copies over the fields from the other element
	 */
	void copyFrom(DataSetElement dse);

	/**
	 * creates a copy of itself
	 */
	DataSetElement createClone();
}

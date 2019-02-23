/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * This class implements the interface for the data set element in which the
 * flag value is significant and present.
 * 
 * @see DataSet
 * @author Nicky Sandhu (DWR).
 * @version $Id: FlaggedDataSetElement.java,v 1.1 2003/10/02 20:49:23 redwood
 *          Exp $
 */
public class FlaggedDataSetElement extends DefaultDataSetElement {
	/**
	 * set flag
	 */
	public final void setFlag(int flag) {
		_flag = flag;
	}

	/**
	 * get flag as string
	 */
	public String getFlagString() {
		return FlagUtils.getQualityFlagName(FlagUtils.getQualityFlag(this))
				+ " | " + FlagUtils.getLastCheckedBy(this);
	}

	/**
	 * get flag
	 */
	public final int getFlag() {
		return _flag;
	}

	/**
	 * copies over the fields from the other element
	 */
	public void copyFrom(DataSetElement dse) {
		if (dse == null)
			return;
		// only two types of elements default and flagged
		super.copyFrom(dse);
		if (dse instanceof FlaggedDataSetElement) {
			_flag = ((FlaggedDataSetElement) dse)._flag;
		}
	}

	/**
	 * creates a copy of itself
	 */
	public DataSetElement createClone() {
		FlaggedDataSetElement e = new FlaggedDataSetElement();
		e.copyFrom(this);
		return e;
	}

	/**
	 * string representation
	 */
	public String toString() {
		StringBuffer buf = new StringBuffer(15 * 3);
		buf.append(super.toString()).append(", ").append(getFlagString());
		return buf.toString();
	}

	/**
	 * the flag
	 */
	private int _flag;
}

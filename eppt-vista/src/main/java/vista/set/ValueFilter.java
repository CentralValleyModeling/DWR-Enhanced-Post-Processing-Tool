/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * Filters out a particular y value
 * 
 * @seeFilterIterator
 * @author Nicky Sandhu
 * @version $Id: ValueFilter.java,v 1.2 1998/10/08 00:04:37 nsandhu Exp $
 */
public class ValueFilter implements ElementFilter {
	/**
	 * Constructor with value to be filtered as argument
	 */
	public ValueFilter(double filterValue) {
		_filterValue = filterValue;
	}

	/**
	 * true if value is acceptable
	 */
	public boolean isAcceptable(DataSetElement dse) {
		return (dse.getY() != _filterValue);
	}

	/**
	 * Value to be filtered
	 */
	private double _filterValue;
}

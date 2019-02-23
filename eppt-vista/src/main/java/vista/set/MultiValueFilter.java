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
 * @version $Id: MultiValueFilter.java,v 1.1 2003/10/02 20:49:27 redwood Exp $
 */
public class MultiValueFilter implements ElementFilter {
	/**
	 * Constructor with value to be filtered as argument
	 */
	public MultiValueFilter(double[] filterValues) {
		_filterValues = filterValues;
	}

	/**
	 * true if value is acceptable
	 */
	public boolean isAcceptable(DataSetElement dse) {
		for (int i = 0; i < _filterValues.length; i++) {
			if (dse.getY() == _filterValues[i])
				return false;
		}
		return true;
	}

	/**
	 * Value to be filtered
	 */
	private double[] _filterValues;
}

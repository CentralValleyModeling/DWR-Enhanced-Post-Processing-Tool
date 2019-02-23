/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * Filters out values equal to Nan
 * 
 * @seeFilterIterator
 * @author Nicky Sandhu
 * @version $Id: NaNFilter.java,v 1.1 2003/10/02 20:49:27 redwood Exp $
 */
public class NaNFilter implements ElementFilter {
	private long _nanValue;

	public NaNFilter() {
		_nanValue = 0x7ff8000000000000L;
	}

	/**
	 * true if value is acceptable
	 */
	public boolean isAcceptable(DataSetElement dse) {
		for (int i = 1; i < dse.getDimension(); i++) {
			if (Double.doubleToLongBits(dse.getX(i)) == _nanValue)
				return false;
		}
		return true;
		// return ( Double.doubleToLongBits(dse.getY()) != _nanValue);
	}
}

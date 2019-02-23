/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * Filters out a value based on its flag being set to the given flag in the
 * constructor
 * 
 * @seeFilterIterator
 * @author Nicky Sandhu
 * @version $Id: FlagFilter.java,v 1.1 2003/10/02 20:49:23 redwood Exp $
 */
public class FlagFilter implements ElementFilter {
	/**
	 * Constructor with flag to be filtered... e.g. to filter out missing flags
	 * FlagUtils.MISSING_FLAG
	 */
	public FlagFilter(int flag) {
		_flag = flag;
	}

	/**
	 * true if value is acceptable
	 */
	public boolean isAcceptable(DataSetElement dse) {
		if (dse instanceof FlaggedDataSetElement
				|| dse instanceof FlaggedTimeElement)
			return FlagUtils.getQualityFlag(dse) != _flag;
		else
			return true;
	}

	/**
	 * flag to be filtered
	 */
	private int _flag;
}

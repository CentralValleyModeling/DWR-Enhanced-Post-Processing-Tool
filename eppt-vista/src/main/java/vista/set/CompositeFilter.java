/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * Filters out values based on the response of other filters. A value is deemed
 * acceptable if and only if it is acceptable by all filters
 * 
 * @seeFilterIterator
 * @author Nicky Sandhu
 * @version $Id: CompositeFilter.java,v 1.1 2003/10/02 20:49:19 redwood Exp $
 */
public class CompositeFilter implements ElementFilter {
	/**
	 * Constructor with filters to be used to filter
	 */
	public CompositeFilter(ElementFilter[] filters) {
		_filters = filters;
	}

	/**
	 * add a filter to the list of filters
	 */
	public void add(ElementFilter filter) {
		ElementFilter[] filters = new ElementFilter[_filters.length + 1];
		System.arraycopy(_filters, 0, filters, 0, _filters.length);
		filters[filters.length - 1] = filter;
		_filters = filters;
	}

	/**
	 * true if value is acceptable
	 */
	public boolean isAcceptable(DataSetElement dse) {
		for (int i = 0; i < _filters.length; i++) {
			if (!_filters[i].isAcceptable(dse))
				return false;
		}
		return true;
	}

	/**
	 * filters to filter by
	 */
	private ElementFilter[] _filters;
}

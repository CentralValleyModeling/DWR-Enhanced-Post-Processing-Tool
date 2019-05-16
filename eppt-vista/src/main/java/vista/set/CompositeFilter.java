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

/**
 * Filters out values based on the response of other filters. A value is deemed
 * acceptable if and only if it is acceptable by all filters
 *
 * @author Nicky Sandhu
 * @version $Id: CompositeFilter.java,v 1.1 2003/10/02 20:49:19 redwood Exp $
 * @seeFilterIterator
 */
public class CompositeFilter implements ElementFilter
{
	/**
	 * filters to filter by
	 */
	private ElementFilter[] _filters;

	/**
	 * Constructor with filters to be used to filter
	 */
	public CompositeFilter(ElementFilter[] filters)
	{
		_filters = filters;
	}

	/**
	 * add a filter to the list of filters
	 */
	public void add(ElementFilter filter)
	{
		ElementFilter[] filters = new ElementFilter[_filters.length + 1];
		System.arraycopy(_filters, 0, filters, 0, _filters.length);
		filters[filters.length - 1] = filter;
		_filters = filters;
	}

	/**
	 * true if value is acceptable
	 */
	public boolean isAcceptable(DataSetElement dse)
	{
		for(int i = 0; i < _filters.length; i++)
		{
			if(!_filters[i].isAcceptable(dse))
			{
				return false;
			}
		}
		return true;
	}
}

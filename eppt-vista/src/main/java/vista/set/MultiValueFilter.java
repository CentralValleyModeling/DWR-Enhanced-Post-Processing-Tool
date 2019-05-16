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
 * Filters out a particular y value
 *
 * @author Nicky Sandhu
 * @version $Id: MultiValueFilter.java,v 1.1 2003/10/02 20:49:27 redwood Exp $
 * @seeFilterIterator
 */
public class MultiValueFilter implements ElementFilter
{
	/**
	 * Value to be filtered
	 */
	private double[] _filterValues;

	/**
	 * Constructor with value to be filtered as argument
	 */
	public MultiValueFilter(double[] filterValues)
	{
		_filterValues = filterValues;
	}

	/**
	 * true if value is acceptable
	 */
	public boolean isAcceptable(DataSetElement dse)
	{
		for(int i = 0; i < _filterValues.length; i++)
		{
			if(dse.getY() == _filterValues[i])
			{
				return false;
			}
		}
		return true;
	}
}

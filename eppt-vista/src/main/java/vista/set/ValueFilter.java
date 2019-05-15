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

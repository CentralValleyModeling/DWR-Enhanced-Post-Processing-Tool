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
 * Filters out to accept only a certain range of values as defined by arguments
 * in constructor
 *
 * @author Nicky Sandhu
 * @version $Id: RangeFilter.java,v 1.2 1998/10/08 00:04:29 nsandhu Exp $
 */
public class RangeFilter implements ElementFilter
{
	/**
	 * max x value
	 */
	private double _xmax;
	/**
	 * min x value
	 */
	private double _xmin;
	/**
	 * max y value
	 */
	private double _ymax;
	/**
	 * min y value
	 */
	private double _ymin;
	/**
	 * Filters out range as given by xmin <= x <= xmax and ymin <= y <= ymax.
	 */
	public RangeFilter(double xmin, double xmax, double ymin, double ymax)
	{
		_xmin = xmin;
		_xmax = xmax;
		_ymin = ymin;
		_ymax = ymax;
	}

	/**
	 * true if value is acceptable
	 */
	public final boolean isAcceptable(DataSetElement dse)
	{
		double xval = dse.getX();
		double yval = dse.getY();
		return (xval >= _xmin && xval <= _xmax)
				&& (yval >= _ymin && yval <= _ymax);
	}
}

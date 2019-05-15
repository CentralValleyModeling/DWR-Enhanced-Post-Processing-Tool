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
package vista.graph;

/**
 * @author Nicky Sandhu
 * @version $Id: PieChartModel.java,v 1.1 2003/10/02 20:49:06 redwood Exp $
 */
public interface PieChartModel
{
	/**
	 *
	 */
	Object getReferenceObject();

	/**
	 *
	 */
	void setReferenceObject(Object obj);

	/**
	 * get title of pie chart
	 */
	String getTitle();

	/**
	 * get maximum value, to scale all values to this value
	 */
	double getSumOfValues();

	/**
	 * true if more value are to follow
	 */
	boolean hasMorePies();

	/**
	 * returns information in the pie model interface
	 */
	PieModel nextPie();

	/**
	 * resets to the beginning
	 */
	void reset();
}

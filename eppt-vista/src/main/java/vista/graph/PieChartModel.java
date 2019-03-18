/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

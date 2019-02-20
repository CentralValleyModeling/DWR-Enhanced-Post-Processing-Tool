/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.graph;

/**
 * Represents the state of selection of a 2D area
 *
 * @author psandhu
 */
public interface RangeSelected
{
	/**
	 * gets the minimum of range on x axis
	 */
	double getXRangeMin();

	/**
	 * gets the maximum of range on x axis
	 */
	double getXRangeMax();

	/**
	 * gets the minimum of range on y axis
	 */
	double getYRangeMin();

	/**
	 * gets the maximum of range on y axis
	 */
	double getYRangeMax();
}

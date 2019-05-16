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

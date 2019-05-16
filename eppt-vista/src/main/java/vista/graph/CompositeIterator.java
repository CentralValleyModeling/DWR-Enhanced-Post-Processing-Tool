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

import java.util.Enumeration;

/**
 * An interface for iterating over the leaves of the Composite interface
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: CompositeIterator.java,v 1.1 2003/10/02 20:48:51 redwood Exp $
 * @see Composite
 * @see Leaf
 */
public interface CompositeIterator extends Enumeration
{
	/**
	 * Resets the iterator to the beginning of data
	 */
	void resetIterator();

	/**
	 * This gets the next Leaf object and advances by one.
	 *
	 * @see Leaf
	 */
	GraphicElement getNext();

	/**
	 * Advance by one.
	 */
	void advance();

	/**
	 * Advance by a specified amount.
	 *
	 * @param n The amount to advance.
	 */
	void advance(int n);

	/**
	 * Return the index of the current position in Composite
	 *
	 * @see Composite
	 */
	int index();
}

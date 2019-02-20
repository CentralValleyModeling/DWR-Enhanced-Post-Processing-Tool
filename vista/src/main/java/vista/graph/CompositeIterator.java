/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

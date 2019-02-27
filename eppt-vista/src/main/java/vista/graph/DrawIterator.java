/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.util.Enumeration;
import java.util.Vector;

import vista.set.DataSetElement;

/**
 * Defines the order of drawing elements
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: DrawIterator.java,v 1.1 2003/10/02 20:48:53 redwood Exp $
 */
public class DrawIterator implements CompositeIterator {
	/**
   *
   */
	public DrawIterator(Vector array) {
		_array = array;
		_index = 0;
		_iterator = _array.elements();
	}

	/**
	 * Resets the iterator to the beginning of data
	 */
	public void resetIterator() {
		_index = 0;
		_iterator = _array.elements();
	}

	/**
	 * This gets the next set of values from the data set.
	 * 
	 * @return The next set of values
	 * @see DataSetElement
	 */
	public GraphicElement getNext() {
		_index++;
		return (GraphicElement) _iterator.nextElement();
	}

	/**
	 * Advance by one.
	 */
	public void advance() {
		_index++;
		_iterator.nextElement();
	}

	/**
	 * Advance by a specified amount.
	 * 
	 * @param n
	 *            The amount to advance.
	 */
	public void advance(int n) {
		for (int i = 0; i < n; i++)
			advance();
	}

	/**
	 * Return the index of my current position.
	 */
	public int index() {
		return _index;
	}

	/**
	 * @return true if more elements are available in the iteration
	 */
	public boolean hasMoreElements() {
		return _iterator.hasMoreElements();
	}

	/**
	 * gets the next element and advances by one.
	 */
	public Object nextElement() {
		_index++;
		return _iterator.nextElement();
	}

	/**
   *
   */
	int _index;
	/**
   *
   */
	Vector _array;
	/**
   *
   */
	Enumeration _iterator = null;
}
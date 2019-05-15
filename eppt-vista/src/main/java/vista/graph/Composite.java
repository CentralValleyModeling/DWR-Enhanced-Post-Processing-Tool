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
 * A composite of bounded objects. Each object contained in this composite has
 * bounds. The composite itself is a bounded object and may be contained in
 * another bounded composite. This is the "Composite" Pattern.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: Composite.java,v 1.1 2003/10/02 20:48:51 redwood Exp $
 */
public interface Composite
{
	/**
	 * adds a graphic element to the composite
	 */
	void add(GraphicElement leaf);

	/**
	 * adds a graphic element to the composite with information
	 */
	void add(String name, GraphicElement leaf);

	/**
	 * inserts an element at the desired index
	 */
	void insertAt(int index, GraphicElement leaf);

	/**
	 * removes the particular object from the container
	 */
	int remove(GraphicElement leaf);

	/**
	 * removes all elements from the composite.
	 */
	void removeAll();

	/**
	 * searches for the first element that matches and returns its index
	 */
	int indexOf(GraphicElement leaf);

	/**
	 * gets the child element count
	 *
	 * @return the number of child graphic elements
	 */
	int getElementCount();

	/**
	 * gets the element at the specified index. This index corresponds to the
	 * order in which the elements were added to this composite.
	 *
	 * @returns the element.
	 */
	GraphicElement getElement(int n);

	/**
	 * returns a copy of the array of graphic elements contained in this
	 * composite.
	 */
	GraphicElement[] getElements();

	/**
	 * gets a iterator object to iterate over the leaves or composites contained
	 * in this composite
	 */
	CompositeIterator getIterator();

	/**
	 * sets the iterator for iterating over the leaves of this composite.
	 */
	void setIterator(CompositeIterator iterator);
}

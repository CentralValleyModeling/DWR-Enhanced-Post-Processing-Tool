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
 * An interface of an iterator which iterates through the elements of a DataSet.
 * Once an iterator hits either end it ignores calls for advancing or retreating
 * past its ends and remains positioned at the extremal elements
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: DataSetIterator.java,v 1.1 2003/10/02 20:49:21 redwood Exp $
 * @see DataSet
 */
public interface DataSetIterator extends java.io.Serializable
{
	/**
	 * Resets the iterator to the beginning of data. This means that the
	 * atStart() method returns true. It also means that getElement() method
	 * returns the first element and that the nextElement() method also returns
	 * the first element. However subsequent calls to the nextElement() method
	 * return the next elements.
	 */
	void resetIterator();

	/**
	 * gets the element at the current location
	 */
	DataSetElement getElement();

	/**
	 * puts the element at the current location
	 */
	void putElement(DataSetElement e);

	/**
	 * positions iterator at index
	 */
	void positionAtIndex(int i);

	/**
	 * Advance by one.
	 */
	void advance();

	/**
	 * Retreat by one
	 */
	void retreat();

	/**
	 * 0 if no elements were skipped by getting this element from the underlying
	 * data set<br>
	 * <p>
	 * + n if the iterator has just skipped n elements of the underlying data
	 * set<br>
	 * <p>
	 * - n if the iterator has just skipped n elements in the reverse direction
	 * of the underlying data set<br>
	 */
	int hasSkipped();

	/**
	 * Gets the current index for the iterator. This keeps track of the number
	 * of advances or retreates that the iterator has made on the underlying
	 * data set. Varies from 0 to size()-1
	 */
	int getIndex();

	/**
	 * This is useful for iterators running on top of other iterators. For
	 * normal iterators its just getIndex()
	 */
	int getUnderlyingIndex();

	/**
	 * if iterator is at start of data
	 */
	boolean atStart();

	/**
	 * if iterator is at end of data.
	 */
	boolean atEnd();

	/**
	 * The maximum of x and y range encapsulated as a data set element.
	 */
	DataSetElement getMaximum();

	/**
	 * The minimum of x and y range encapsulated as a data set element.
	 */
	DataSetElement getMinimum();
}

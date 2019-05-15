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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Iterator;

/**
 * Uses a regular expression to filter a group of pathnames. It defines either
 * Perl5 or Awk Expressions
 */
public class DataReferenceFilter implements Enumeration<DataReference> {
	/**
	 * Iterates over an array of data references using a filter as defined by
	 * regex
	 */
	public DataReferenceFilter(DataReference[] refs, Predicate<DataReference> predicate) {
		_filteringFunction = predicate;
		setSelecting(true);
		setReferences(refs);
		filter();
	}

	/**
	 * sets the references and filters them.
	 */
	public void setReferences(DataReference[] refs) {
		_references = refs;
	}

	/**
	 * set to true if values are to be selected according to filtering function
	 * criteria or false for rejecting. Selecting is the default.
	 */
	public final void setSelecting(boolean select) {
		_selecting = select;
	}

	/**
	 * true if values are selected accorrding to filtering function.
	 */
	public final boolean isSelecting() {
		return _selecting;
	}

	/**
	 * true if it has more elements
	 */
	public boolean hasMoreElements() {
		return iterator.hasNext();
	}

	/**
	 * next element in the sequence
	 */
	public DataReference nextElement() {
		return iterator.next();
	}

	/**
	 * filters the methods
	 */
	public final void filter() {
		_filtered = true;
		CollectionUtils.filter(Arrays.asList(_references), _filteringFunction, _selecting);
		resetIterator();
	}

	/**
	 * returns the filtered array of references
	 */
	public DataReference[] getFilteredArray() {
		DataReference[] refs = new DataReference[_filteredArray.size()];
		return _filteredArray.toArray(refs);
	}

	/**
	 * resets the iterator.
	 */
	public void resetIterator() {
		iterator = _filteredArray.iterator();
	}

	/**
	 * true if filter selects by given criteria
	 */
	private boolean _selecting;
	/**
	 * true if filtering is already done.
	 */
	private boolean _filtered;
	/**
	 * array containing the filtered result.
	 */
	private ArrayList<DataReference> _filteredArray;
	/**
	 * The data reference array
	 */
	private DataReference[] _references;
	/**
	 * The function used to define what values are to be rejected
	 */
	private Predicate<DataReference> _filteringFunction;
	/**
	 * iterator for the filtered array.
	 */
	private Iterator<DataReference> iterator;
}

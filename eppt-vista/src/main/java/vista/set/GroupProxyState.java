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

import java.util.Comparator;

/**
 * This a proxy for group object
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupProxyState.java,v 1.1 2003/10/02 20:49:24 redwood Exp $
 */
public abstract class GroupProxyState extends Group {
	/**
	 * initializes state object with given proxy. This proxy is the initial
	 * delegate and informs this state object on being initialized.
	 */
	public GroupProxyState(GroupProxy proxy) {
		proxy.addStateListener(this);
		_delegate = proxy;
	}

	/**
	 * get number of data references
	 */
	public int getNumberOfDataReferences() {
		return _delegate.getNumberOfDataReferences();
	}

	/**
	 * gets data reference by index
	 */
	public DataReference getDataReference(int index) {
		return _delegate.getDataReference(index);
	}

	/**
	 * gets data reference by name
	 */
	public DataReference getDataReference(String dataName) {
		return _delegate.getDataReference(dataName);
	}

	/**
	 * gets all the data references
	 */
	public DataReference[] getAllDataReferences() {
		return _delegate.getAllDataReferences();
	}

	/**
	 * copies this group into given group.
	 */
	public void copyInto(Group group) {
		_delegate.copyInto(group);
	}

	/**
	 * returns a copy of itself.
	 */
	public Object clone() {
		return _delegate.clone();
	}

	/**
	 * Creates a new group which is the union of this group and the specified
	 * groupg
	 */
	public Group unionWith(Group group) {
		return _delegate.unionWith(group);
	}

	/**
	 * Creates a new group which is the intersection of this group and the
	 * specified group. Returns null if intersection is the null set.
	 */
	public Group intersectionWith(Group group) {
		return _delegate.intersectionWith(group);
	}

	/**
	 * Sorts group by calling upon the SortAlgorithm to return an array of data
	 * references by some criteria. The reference list is being directly
	 * manipulated by the sorting mechanism.
	 */
	public void sortBy(Comparator<DataReference> comparator) {
		_delegate.sortBy(comparator);
	}

	/**
	 * filters on this group using filter. This causes the reference list held
	 * by this group to be modified. This change is physically reflected by
	 * appending the filter expression string to the name of this group.
	 */
	public void filterBy(Predicate predicate, boolean selecting) {
		_delegate.filterBy(predicate, selecting);
	}

	/**
	 * point delegate to the new group object.
	 */
	void groupIsInitialized() {
		Group g = new Group();
		_delegate.copyInto(g);
		_delegate = g;
	}

	/**
	 * true if initialized
	 */
	private boolean _initialized = false;
	/**
	 * state object listening for initialization on this proxy
	 */
	private Group _delegate;
}

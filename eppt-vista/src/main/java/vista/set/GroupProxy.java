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
 * A proxy for the group class
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupProxy.java,v 1.1 2003/10/02 20:49:24 redwood Exp $
 */
public abstract class GroupProxy extends Group {
	/**
	 * returns the initialized group object.
	 */
	protected abstract Group getInitializedGroup();

	/**
	 * get number of data references
	 */
	public int getNumberOfDataReferences() {
		if (!_initialized) {
			initializeGroup();
		}
		return super.getNumberOfDataReferences();
	}

	/**
   *
   */
	public void reload() {
		_initialized = false;
	}

	/**
	 * gets data reference by index
	 */
	public DataReference getDataReference(int index) {
		if (!_initialized) {
			initializeGroup();
		}
		return super.getDataReference(index);
	}

	/**
	 * gets data reference by name
	 */
	public DataReference getDataReference(String dataName) {
		if (!_initialized) {
			initializeGroup();
		}
		return super.getDataReference(dataName);
	}

	/**
	 * gets all the data references
	 */
	public DataReference[] getAllDataReferences() {
		if (!_initialized) {
			initializeGroup();
		}
		return super.getAllDataReferences();
	}

	/**
	 * copies this group into given group.
	 */
	public void copyInto(Group group) {
		if (!_initialized) {
			initializeGroup();
		}
		super.copyInto(group);
	}

	/**
	 * returns a copy of itself.
	 */
	public Object clone() {
		if (!_initialized) {
			initializeGroup();
		}
		return super.clone();
	}

	/**
	 * Creates a new group which is the union of this group and the specified
	 * groupg
	 */
	public Group unionWith(Group group) {
		if (!_initialized) {
			initializeGroup();
		}
		return super.unionWith(group);
	}

	/**
	 * Creates a new group which is the intersection of this group and the
	 * specified group. Returns null if intersection is the null set.
	 */
	public Group intersectionWith(Group group) {
		if (!_initialized) {
			initializeGroup();
		}
		return super.intersectionWith(group);
	}

	/**
	 * Sorts group by calling upon the SortAlgorithm to return an array of data
	 * references by some criteria. The reference list is being directly
	 * manipulated by the sorting mechanism.
	 */
	public void sortBy(Comparator<DataReference> sortAlgo) {
		if (!_initialized) {
			initializeGroup();
		}
		super.sortBy(sortAlgo);
	}

	/**
	 * filters on this group using filter. This causes the reference list held
	 * by this group to be modified. This change is physically reflected by
	 * appending the filter expression string to the name of this group.
	 */
	public void filterBy(Predicate<DataReference> predicate, boolean selecting) {
		if (!_initialized) {
			initializeGroup();
		}
		super.filterBy(predicate, selecting);
	}

	/**
	 * adds state listener
	 */
	void addStateListener(GroupProxyState g) {
		_stateListener = g;
	}

	/**
	 * informs state listener
	 */
	void informStateListener() {
		if (_stateListener != null) {
			_stateListener.groupIsInitialized();
		}
	}

	/**
	 * initialize group
	 */
	private void initializeGroup() {
		Group g = getInitializedGroup();
		g.copyInto(this);
		_initialized = true;
		informStateListener();
	}

	/**
	 * true if initialized
	 */
	private boolean _initialized = false;
	/**
	 * state object listening for initialization on this proxy
	 */
	private GroupProxyState _stateListener;
}

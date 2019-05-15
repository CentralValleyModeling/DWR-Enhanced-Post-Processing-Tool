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
 * Sorts an array of named by their names The default sorting order is
 * increasing.
 * 
 * @author Nicky Sandhu
 * @version $Id: NameSort.java,v 1.1 2003/10/02 20:49:27 redwood Exp $
 */
public class GroupSortMechanism implements SortMechanism<Group> {
	private int sortOrder;

	/**
	 * sorts using the partId specified and increasing order of sort.
	 */
	public GroupSortMechanism() {
		this(INCREASING);
	}

	/**
	 * Initializes the sorting method for a certain part id.
	 * 
	 * @param partId
	 *            Id of the part by which to sort as defined by Pathname.?_PART
	 *            constants
	 * @see Pathname
	 */
	public GroupSortMechanism(int sortOrder) {
		this.sortOrder = sortOrder;
	}

	/**
	 * checks if order of sort is ascending or descending...
	 */
	public boolean isAscendingOrder() {
		return sortOrder == INCREASING;
	}

	/**
	 * sets ascending / descending order
	 */
	public void setAscendingOrder(boolean ascending) {
		sortOrder = ascending ? INCREASING : DECREASING; 
	}
	@Override
	public int compare(Group o1, Group o2) {
		if (isAscendingOrder()){
			return o1.getName().compareTo(o2.getName());
		} else {
			return o2.getName().compareTo(o1.getName());
		}
	}
}

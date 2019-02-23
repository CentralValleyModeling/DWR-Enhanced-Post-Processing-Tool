/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

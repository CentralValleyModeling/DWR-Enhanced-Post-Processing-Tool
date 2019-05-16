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
 * Interface extension of Binary predicate
 *
 * @param <T>
 * @author Nicky Sandhu
 * @version $Id: SortMechanism.java,v 1.1 2003/10/02 20:49:32 redwood Exp $
 */
public interface SortMechanism<T> extends Comparator<T>
{
	/**
	 * increasing order of sort
	 */
	int INCREASING = 1;
	/**
	 * decreasing order of sort
	 */
	int DECREASING = 2;

	/**
	 * true if sort order is ascending
	 */
	boolean isAscendingOrder();

	/**
	 * sets the order to ascending or descending.
	 */
	void setAscendingOrder(boolean ascending);

}

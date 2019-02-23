/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

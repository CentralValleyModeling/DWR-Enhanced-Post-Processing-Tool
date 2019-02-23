/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import java.util.ArrayList;
import java.util.Comparator;

/**
 * Interface to sorting algorithms.
 *
 * @author Nicky Sandhu
 * @version $Id: Sorter.java,v 1.1 2003/10/02 20:49:32 redwood Exp $
 */
public interface Sorter<T> extends Comparator<T>
{
	/**
	 *
	 */
	void setSortMechanism(SortMechanism<T> sm);

	/**
	 * Takes an array of data references and returns a new array of sorted data
	 * references.
	 */
	ArrayList<? extends T> sort(ArrayList<? extends T> refs);
}

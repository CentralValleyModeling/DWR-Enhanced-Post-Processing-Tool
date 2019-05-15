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

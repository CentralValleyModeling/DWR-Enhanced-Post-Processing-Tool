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
 * compares named objects
 * 
 * @see Named
 */
public class EqualName implements Comparator<Named> {
	/**
	 * execute function returns true if object1's name is lexicographically less
	 * than object2's name
	 */
	@Override
	public int compare(Named o1, Named o2) {
		return o1.getName().compareTo(o2.getName());
	}
}

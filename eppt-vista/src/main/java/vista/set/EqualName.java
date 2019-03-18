/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

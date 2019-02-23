/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import java.util.Comparator;

/**
 * Compares names of Named objects and returns true if object1
 * name is less than object 2 name.
 * @see Named
 * @author Nicky Sandhu
 * @version $Id: LessEqualName.java,v 1.2 1998/10/08 00:04:22 nsandhu Exp $
 */
public class LessEqualName implements Comparator<Named> {
	/**
   *
   */
	public LessEqualName() {
	}

	@Override
	/**
	 * execute function returns true if object1's name is lexicographically less
	 * than object2's name
	 */
	public int compare(Named o1, Named o2) {
		if (o1==null){
			return -1;
		}
		if (o2 == null){
			return 1;
		}
		if (o1 == null && o2 == null){
			return 0;
		}
		return o1.getName().compareTo(o2.getName());
	}

}

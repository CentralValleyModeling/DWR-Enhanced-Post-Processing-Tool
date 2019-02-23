/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;


/**
 * returns true if object is null. Useful in filtering containers of null
 * values.
 */
public class IsNull implements Predicate<Object> {
	/**
	 * empty constructor
	 */
	public IsNull() {
	}

	@Override
	public boolean apply(Object type) {
		return type==null;
	}
}

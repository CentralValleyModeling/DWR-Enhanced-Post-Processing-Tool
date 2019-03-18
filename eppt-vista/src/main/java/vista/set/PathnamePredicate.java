/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;


/**
 * Function for filtering the entire pathname string using a regular expressioin
 */
@SuppressWarnings("serial")
public class PathnamePredicate extends RegExPredicate<DataReference> {
	/**
	 * initializes the regular expression compilers
	 */
	public PathnamePredicate(String regex) {
		super(regex);
	}


	@Override
	public boolean apply(DataReference ref) {
		return (ref != null && _pattern.matcher(ref.getPathname()
						.getFullPath()).find());
	}
}

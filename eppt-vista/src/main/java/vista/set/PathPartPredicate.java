/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * Matches the given regular expression to a certain pathname part.
 * 
 */
@SuppressWarnings("serial")
public class PathPartPredicate extends RegExPredicate<DataReference> {
	/**
	 * initializes the regular expression compilers
	 */
	public PathPartPredicate(String regex, int partId) {
		// check (partId);
		super(regex);
		_partId = partId;
	}

	/**
	 * returns the path part id.
	 */
	public int getPartId() {
		return _partId;
	}

	/**
	 * The path part id.
	 */
	private int _partId;

	@Override
	public boolean apply(DataReference first) {
		return (first != null)
				&& (first.getPathname().getPart(_partId).length() > 0)
				&& (_pattern.matcher(first.getPathname().getPart(_partId))
						.find());
	}
}

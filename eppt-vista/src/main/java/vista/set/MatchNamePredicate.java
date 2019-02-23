/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;


/**
 * Matches name of Named implementers to name given in constructor.
 * 
 * @see Named
 */
public class MatchNamePredicate implements Predicate<Named> {
	/**
	 * name to be matched
	 */
	private String _name;
	/**
	 * constructor name to be matched
	 */
	public MatchNamePredicate(String name) {
		_name = name;
	}


	/**
	 * execute function returns true if object's name matches given name.
	 */
	@Override
	public boolean apply(Named type) {
		return type != null && type.getName().equals(_name);
	}
}

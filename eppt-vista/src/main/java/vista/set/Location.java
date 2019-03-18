/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import java.util.ArrayList;

/**
 * Describes a location.
 * 
 * @author Nicky Sandhu
 * @version $Id: Location.java,v 1.1 2003/10/02 20:49:26 redwood Exp $
 */
public class Location {
	/**
	 * coordinates
	 */
	private double _x, _y;
	private String _name;
	private ArrayList<String> _aliases;

	/**
	 * creates a location with given name, x and y coordinates
	 */
	public Location(String name, double x, double y) {
		_name = name;
		_aliases = new ArrayList<String>();
		_x = x;
		_y = y;
	}

	/**
	 * @return the x coordinate
	 */
	public double getX() {
		return _x;
	}

	/**
	 * @return the y coordinate
	 */
	public double getY() {
		return _y;
	}

	/**
	 * @return the name of location
	 */
	public String getName() {
		return _name;
	}

	/**
	 * adds an alias to this location
	 */
	public void addAlias(String alias) {
		_aliases.add(alias);
	}

	/**
	 * returns the list of aliases for this location.
	 */
	public String[] getAliases() {
		String[] names = new String[_aliases.size()];
		return _aliases.toArray(names);
	}
}

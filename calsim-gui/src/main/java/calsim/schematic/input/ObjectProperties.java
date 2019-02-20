/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.schematic.input;

/**
 * This class is the superclass of all the property classes of objects
 * in the schematic.
 */
public interface ObjectProperties
{
	/**
	 * returns the name of this object
	 */
	String getName();

	/**
	 * returns the description
	 */
	String getDescription();

	/**
	 * returns a data source for the given property name.
	 * E.g. for the channel property name = "MINIMUM FLOW" this method
	 * returns the data source object representing that property.
	 */
	DataSource getDataSource(String propertyName);

	/**
	 *
	 */
	String[] getAllPropertyNames();
}





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





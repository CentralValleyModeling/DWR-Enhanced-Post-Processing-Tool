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

import java.util.Enumeration;
import java.util.Hashtable;

/**
 * This class contains all the informations about an object, and manipulates the properties
 * of the objects in the schematic.
 *
 * @author Yan-Ping Zuo
 * @version $Id: InputObject.java, v 1.0 12/21/1999
 */
public class InputObject
{
	private String _name;
	private String _type;
	private String _descript;
	private Hashtable _dataSourceTable, _stringPropTable;

	/**
	 *
	 */
	public InputObject(String name)
	{
		setName(name);
		_dataSourceTable = new Hashtable();
		_stringPropTable = new Hashtable();
	}

	/**
	 * returns the name of this object
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * sets the name of this object
	 */
	public void setName(String name)
	{
		_name = name.toUpperCase();
	}

	/**
	 * returns the type of this object
	 */
	public String getType()
	{
		return _type;
	}

	/**
	 * sets the type of this object
	 */
	public void setType(String type)
	{
		_type = type.toUpperCase();
	}

	/**
	 * returns the description
	 */
	public String getDescription()
	{
		return _descript;
	}

	/**
	 * sets the description
	 */
	public void setDescription(String descript)
	{
		_descript = descript;
	}

	/**
	 * returns a data source for the given property name.
	 * E.g. for the channel property name = "MINIMUM FLOW" this method
	 * returns the data source object representing that property.
	 */
	public DataSource getDataSource(String propertyName)
	{
		return (DataSource) _dataSourceTable.get(propertyName.toUpperCase());
	}

	/**
	 * returns the names of all data source properties
	 */
	public String[] getAllDataSourcePropertyNames()
	{
		String[] propNames = new String[_dataSourceTable.size()];
		int i = 0;
		for(Enumeration e = _dataSourceTable.keys(); e.hasMoreElements(); i++)
		{
			propNames[i] = (String) e.nextElement();
		}
		return propNames;
	}

	/**
	 * returns the string property for the given property name
	 */
	public String getStringProperty(String propertyName)
	{
		return (String) _stringPropTable.get(propertyName.toUpperCase());
	}

	/**
	 * returns the list of all string properties
	 */
	public String[] getAllStringPropertyNames()
	{
		String[] propNames = new String[_stringPropTable.size()];
		int i = 0;
		for(Enumeration e = _stringPropTable.keys(); e.hasMoreElements(); i++)
		{
			propNames[i] = (String) e.nextElement();
		}
		return propNames;
	}

	/**
	 * sets a data source for the given property name.
	 * E.g. for the channel property name = "MINIMUM FLOW" this method
	 * sets the data source object representing that property.
	 */
	public void setDataSource(String propertyName, DataSource dataSource)
	{
		_dataSourceTable.put(propertyName.toUpperCase(), dataSource);
	}

	/**
	 * sets the string property for the given property name
	 */
	public void setStringProperty(String propertyName, String property)
	{
		_stringPropTable.put(propertyName.toUpperCase(), property);
	}

	/*
	 * deletes data source with given property name if present else
	 * it throws an exception.        //??returns silently.
	 */
	public void deleteDataSource(String propertyName)
	{
		String name = propertyName.toUpperCase();
		if(_dataSourceTable.containsKey(name))
		{
			_dataSourceTable.remove(name);
		}
		else
		{
			throw new RuntimeException("Property name: " + name + " does not exist.");
		}
	}

	/**
	 * deletes data source with given property name if present else
	 * it throws an exception.        //??returns silently.
	 */
	public void deleteStringProperty(String propertyName)
	{
		String name = propertyName.toUpperCase();
		if(_stringPropTable.containsKey(name))
		{
			_stringPropTable.remove(name);
		}
		else
		{
			throw new RuntimeException("Property name: " + name + " does not exist.");
		}
	}
}





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


package calsim.wreslcoder.wresl;

/**
 * Contains parser parameters associated with input DSS timeseries variables.
 *
 * @author Armin Munevar
 * @version $Id: Timeseries.java,v 1.1.2.3 2001/07/12 02:00:10 amunevar Exp $
 */
public class Timeseries
{

	//	private String _name,_bpart,_cpart,_units,_convert;
	private String _name, _units, _convert;


	/**
	 * Creates a new instance with default values
	 */
	public Timeseries()
	{
		_name = "";
		//		_bpart = new String();
		//		_cpart = new String();
		_units = "UNKNOWN";
		_convert = "UNKNOWN";
	}

	/**
	 * Creates a new instance with specified name, bpart, cpart, units, and convert units
	 */
	public Timeseries(String name, String bpart, String cpart, String units, String convert)
	{
		_name = name.toUpperCase();
		//		_bpart = bpart.toUpperCase();
		//		_cpart = cpart.toUpperCase();
		_units = units.toUpperCase();
		_convert = convert.toUpperCase();
	}

	/**
	 * Gets name of timeseries
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * Sets name of timeseries
	 */
	public void setName(String name)
	{
		_name = name.toUpperCase();
	}

	/**
	 * Gets units of timeseries
	 */
	public String getUnits()
	{
		return _units;
	}

	/**
	 * Sets units of timeseries
	 */
	public void setUnits(String units)
	{
		_units = units.toUpperCase();
	}

	/**
	 * Gets conversion units for timeseries
	 */
	public String getConvert()
	{
		return _convert;
	}

	/**
	 * Sets conversion units for timeseries
	 */
	public void setConvert(String convert)
	{
		_convert = convert.toUpperCase();
	}

}

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
package vista.db.dss;

/**
 * A class encapsulating information about data from the DSS data base.
 * 
 * @author Nicky Sandhu
 * @version $Id: DSSData.java,v 1.1 2003/10/02 20:48:44 redwood Exp $
 */
public class DSSData {
	/**
	 * The type of data as defined by constants in DSSUtil class
	 * 
	 * @see DSSUtil
	 */
	public int _dataType;
	/**
	 * The x values
	 */
	public double[] _xValues;
	/**
	 * The y values
	 */
	public double[] _yValues;
	/**
	 * the offset if any;
	 */
	public int _offset = 0;
	/**
	 * The flag values
	 */
	public int[] _flags;
	/**
	 * The number read, also the size of all the data/flag arrays
	 */
	public int _numberRead;
	/**
	 * a string representation of the type of x values
	 */
	public String _xType;
	/**
	 * a string representation of the type of y values
	 */
	public String _yType;
	/**
	 * a string representation of the unit of x values
	 */
	public String _xUnits;
	/**
	 * a string representation of the unit of y values
	 */
	public String _yUnits;
}

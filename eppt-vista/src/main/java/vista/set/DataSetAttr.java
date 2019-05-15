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
package vista.set;

/**
 * Encapsulates the attributes of the data set. These attributes are Type of
 * data: integer : DataType.REGULAR_TIME_SERIES | IRREGULAR_TIME_SERIES | PAIRED
 * | TEXT | UNDEFINED Units: String: X dimension and Y dimension Group Name :
 * String: A part Location Name: String: B part Type Name : String: C part
 * Source Name : String: F part The D and E parts are obtained from the DataSet
 * type
 *
 * @author Nicky Sandhu
 * @version $Id: DataSetAttr.java,v 1.1 2003/10/02 20:49:21 redwood Exp $
 */
public class DataSetAttr implements java.io.Serializable
{
	/**
	 *
	 */
	private int _type;
	private String _xType, _yType;
	private String _xUnits, _yUnits;
	private String _groupName, _locationName, _typeName, _sourceName;

	/**
	 * construct a new attribute object with unit units and type type
	 */
	public DataSetAttr(int type, String xUnits, String yUnits, String xType,
					   String yType)
	{
		this(null, null, null, null, type, xUnits, yUnits, xType, yType);
	}

	/**
	 * construct a new attribute object with unit units and type type
	 */
	public DataSetAttr(String groupName, String locName, String typeName,
					   String sourceName, int type, String xUnits, String yUnits,
					   String xType, String yType)
	{
		setType(type);
		setXUnits(xUnits);
		setYUnits(yUnits);
		setXType(xType);
		setYType(yType);
		setGroupName(groupName);
		setLocationName(locName);
		setTypeName(typeName);
		setSourceName(sourceName);
	}

	/**
	 *
	 */
	public DataSetAttr createClone()
	{
		DataSetAttr attr = new DataSetAttr(_groupName, _locationName,
				_typeName, _sourceName, _type, _xUnits, _yUnits, _xType, _yType);
		return attr;
	}

	/**
	 * what is generally referred to as the A part
	 */
	public String getGroupName()
	{
		return _groupName;
	}

	/**
	 * what is generally referred to as the A part
	 */
	public void setGroupName(String name)
	{
		_groupName = name == null ? "" : name.toUpperCase();
	}

	/**
	 * the b part
	 */
	public String getLocationName()
	{
		return _locationName;
	}

	/**
	 * the b part
	 */
	public void setLocationName(String name)
	{
		_locationName = name == null ? "" : name.toUpperCase();
	}

	/**
	 * the c part
	 */
	public String getTypeName()
	{
		return _typeName;
	}

	/**
	 * the c part
	 */
	public void setTypeName(String name)
	{
		_typeName = name == null ? "" : name.toUpperCase();
	}

	/**
	 * the f part
	 */
	public String getSourceName()
	{
		return _sourceName;
	}

	/**
	 * the f part
	 */
	public void setSourceName(String name)
	{
		_sourceName = name == null ? "" : name.toUpperCase();
	}

	/**
	 * get string representing the units
	 *
	 * @see Units
	 */
	public String getXUnits()
	{
		return _xUnits;
	}

	/**
	 * set string representing the units
	 *
	 * @see Units
	 */
	public void setXUnits(String units)
	{
		_xUnits = units == null ? "" : units.toUpperCase();
	}

	/**
	 *
	 */
	public String getXType()
	{
		return _xType;
	}

	/**
	 *
	 */
	public void setXType(String type)
	{
		_xType = type == null ? "" : type.toUpperCase();
	}

	/**
	 *
	 */
	public String getYType()
	{
		return _yType;
	}

	/**
	 *
	 */
	public void setYType(String type)
	{
		_yType = type == null ? "" : type.toUpperCase();
	}

	/**
	 *
	 */
	public String getYUnits()
	{
		return _yUnits;
	}

	/**
	 *
	 */
	public void setYUnits(String units)
	{
		_yUnits = units == null ? "" : units.toUpperCase();
	}

	/**
	 * get integer representing type
	 *
	 * @see DataType
	 */
	public int getType()
	{
		return _type;
	}

	/**
	 * set integer representing type
	 *
	 * @see DataType
	 */
	public void setType(int type)
	{
		_type = type;
	}

	/**
	 * A string representation
	 */
	public String toString()
	{
		StringBuffer buffer = new StringBuffer();
		buffer.append(this.getTypeName() + " @ " + this.getLocationName() + "\n");
		buffer.append("Source: " + this.getSourceName() + "\n");
		buffer.append("Group: " + this.getGroupName() + "\n");
		buffer.append("Type: " + this.getType() + "\n");
		buffer.append("X (type,units): " + this.getXType() + "," + this.getXUnits() + "\n");
		buffer.append("Y (type,units): " + this.getYType() + "," + this.getYUnits() + "\n");
		return buffer.toString();
	}
}

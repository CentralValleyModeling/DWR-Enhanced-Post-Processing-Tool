/* * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019. * * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed * under the GNU General Public License, version 2. This means it can be * copied, distributed, and modified freely, but you may not restrict others * in their ability to copy, distribute, and modify it. See the license below * for more details. * * GNU General Public License */package vista.report;import vista.set.DataSetAttr;/** * A set of data attributes that contains original units field. */public class TSDataAttr extends DataSetAttr{	private String _originalUnits;	/**	 *	 */	public TSDataAttr(DataSetAttr attr)	{		super(attr.getGroupName(),				attr.getLocationName(),				attr.getTypeName(),				attr.getSourceName(),				attr.getType(),				attr.getXUnits(),				attr.getYUnits(),				attr.getXType(),				attr.getYType());		setOriginalUnits(attr.getYUnits());	}	/**	 *	 */	public String getOriginalUnits()	{		return _originalUnits;	}	/**	 *	 */	private void setOriginalUnits(String units)	{		_originalUnits = units;	}}
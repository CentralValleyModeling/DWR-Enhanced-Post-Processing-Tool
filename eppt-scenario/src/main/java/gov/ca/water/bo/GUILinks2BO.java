/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.bo;
//! Representation of GUI_Links2  (GUI -> .table) record

/**
 * This is used to hold the information of GUI_Link2.table in memory. Please
 * refer GUI_Link2.table for more information.
 *
 * @author Mohan
 */
public class GUILinks2BO
{
	/**
	 * The id of the gui from GUI_Link2.table. This is control value from the
	 * gui.xml file.
	 */
	private String _guiId;
	/**
	 * The table name from GUI_Link2.table. This is the file name which the
	 * index and option are going to be stored.
	 */
	private String _tableName;
	/**
	 * The index from GUI_Link2.table. The value which is return to the table
	 * name file.
	 */
	private String _index;
	/**
	 * The option from GUI_Link2.table. The value which is return to the table
	 * name file.
	 */
	private String _option;
	/**
	 * The description from GUI_Link2.table. The value which is return to the
	 * table name file.
	 */
	private String _description;
	/**
	 * The dashboard from GUI_Link2.table. This will tell this control belong to
	 * which dashboard.
	 */
	private String _dashboard;
	/**
	 * The dataTables from GUI_Link2.table. Please refer GUI_Link2.table for
	 * more information.
	 */
	private String _dataTables;
	/**
	 * The switchID from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String _switchID;
	/**
	 * The tableID from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String _tableID;
	/**
	 * The d1485d1641 from GUI_Link2.table. Please refer GUI_Link2.table for
	 * more information.
	 */
	private String _d1485d1641;
	/**
	 * The d1641 from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String _d1641;
	/**
	 * The noregulation from GUI_Link2.table. Please refer GUI_Link2.table for
	 * more information.
	 */
	private String _noregulation;
	/**
	 * The userDefined from GUI_Link2.table. Please refer GUI_Link2.table for
	 * more information.
	 */
	private String _userDefined;
	/**
	 * The d1485 from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String _d1485;
	/**
	 * The regID from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String _regID;

	public GUILinks2BO(String guiId, String tableName, String index, String option, String description,
					   String dashboard,
					   String dataTables, String switchID, String tableID, String d1485d1641, String d1641,
					   String noregulation,
					   String userDefined, String d1485, String regID)
	{
		this._guiId = guiId;
		this._tableName = tableName;
		this._index = index;
		this._option = option;
		this._description = description;
		this._dashboard = dashboard;
		this._dataTables = dataTables;
		this._switchID = switchID;
		this._tableID = tableID;
		this._d1485d1641 = d1485d1641;
		this._d1641 = d1641;
		this._noregulation = noregulation;
		this._userDefined = userDefined;
		this._d1485 = d1485;
		this._regID = regID;
	}

	public String getGuiId()
	{
		return _guiId;
	}

	public String getTableName()
	{
		return _tableName;
	}

	public String getIndex()
	{
		return _index;
	}

	public String getOption()
	{
		return _option;
	}

	public String getDescription()
	{
		return _description;
	}

	public String getDashboard()
	{
		return _dashboard;
	}

	public String getDataTables()
	{
		return _dataTables;
	}

	public String getSwitchID()
	{
		return _switchID;
	}

	public String getTableID()
	{
		return _tableID;
	}

	public String getD1485D1641()
	{
		return _d1485d1641;
	}

	public String getD1641()
	{
		return _d1641;
	}

	public String getNoregulation()
	{
		return _noregulation;
	}

	public String getUserDefined()
	{
		return _userDefined;
	}

	public String getD1485()
	{
		return _d1485;
	}

	public String getRegID()
	{
		return _regID;
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
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
	private String guiId;
	/**
	 * The table name from GUI_Link2.table. This is the file name which the
	 * index and option are going to be stored.
	 */
	private String tableName;
	/**
	 * The index from GUI_Link2.table. The value which is return to the table
	 * name file.
	 */
	private String index;
	/**
	 * The option from GUI_Link2.table. The value which is return to the table
	 * name file.
	 */
	private String option;
	/**
	 * The description from GUI_Link2.table. The value which is return to the
	 * table name file.
	 */
	private String description;
	/**
	 * The dashboard from GUI_Link2.table. This will tell this control belong to
	 * which dashboard.
	 */
	private String dashboard;
	/**
	 * The dataTables from GUI_Link2.table. Please refer GUI_Link2.table for
	 * more information.
	 */
	private String dataTables;
	/**
	 * The switchID from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String switchID;
	/**
	 * The tableID from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String tableID;
	/**
	 * The d1485d1641 from GUI_Link2.table. Please refer GUI_Link2.table for
	 * more information.
	 */
	private String d1485d1641;
	/**
	 * The d1641 from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String d1641;
	/**
	 * The noregulation from GUI_Link2.table. Please refer GUI_Link2.table for
	 * more information.
	 */
	private String noregulation;
	/**
	 * The userDefined from GUI_Link2.table. Please refer GUI_Link2.table for
	 * more information.
	 */
	private String userDefined;
	/**
	 * The d1485 from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String d1485;
	/**
	 * The regID from GUI_Link2.table. Please refer GUI_Link2.table for more
	 * information.
	 */
	private String regID;

	public GUILinks2BO(String guiId, String tableName, String index, String option, String description,
					   String dashboard,
					   String dataTables, String switchID, String tableID, String d1485d1641, String d1641,
					   String noregulation,
					   String userDefined, String d1485, String regID)
	{
		this.guiId = guiId;
		this.tableName = tableName;
		this.index = index;
		this.option = option;
		this.description = description;
		this.dashboard = dashboard;
		this.dataTables = dataTables;
		this.switchID = switchID;
		this.tableID = tableID;
		this.d1485d1641 = d1485d1641;
		this.d1641 = d1641;
		this.noregulation = noregulation;
		this.userDefined = userDefined;
		this.d1485 = d1485;
		this.regID = regID;
	}

	public String getGuiId()
	{
		return guiId;
	}

	public String getTableName()
	{
		return tableName;
	}

	public String getIndex()
	{
		return index;
	}

	public String getOption()
	{
		return option;
	}

	public String getDescription()
	{
		return description;
	}

	public String getDashboard()
	{
		return dashboard;
	}

	public String getDataTables()
	{
		return dataTables;
	}

	public String getSwitchID()
	{
		return switchID;
	}

	public String getTableID()
	{
		return tableID;
	}

	public String getD1485D1641()
	{
		return d1485d1641;
	}

	public String getD1641()
	{
		return d1641;
	}

	public String getNoregulation()
	{
		return noregulation;
	}

	public String getUserDefined()
	{
		return userDefined;
	}

	public String getD1485()
	{
		return d1485;
	}

	public String getRegID()
	{
		return regID;
	}
}
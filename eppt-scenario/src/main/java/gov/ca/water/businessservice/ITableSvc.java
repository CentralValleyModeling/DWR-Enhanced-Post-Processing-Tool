/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice;

import java.util.List;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.DataTableModel;

/**
 * This is the interface that handles all the tables like
 * gui_xchanneldays.table, gui_EiRatio.table etc.
 *
 * @author Mohan
 */
public interface ITableSvc
{

	/**
	 * This method will take the Table Name and return the DataTableModle object
	 * of that table.
	 *
	 * @param tableName Name of the table with the option like D1641 or D1485.
	 * @return It will return the table as {@link DataTableModel}.
	 * @throws CalLiteGUIException When loading the tables if it gets an error it will throw it.
	 */
	DataTableModel getTable(String tableName) throws CalLiteGUIException;

	/**
	 * This method will take the Table Name and the function that gets the data
	 * and return the DataTableModle object of that table.
	 *
	 * @param tableName Just the table name without the path and extension.
	 * @param function  The function which retrieve the data from the table file.
	 * @return The {@link DataTableModel} object with whole table.
	 * @throws CalLiteGUIException If anything wrong about the file then it will throw a
	 *                             exception with the information about it.
	 */
	DataTableModel getTable(String tableName, TwoFunction<List<String>, Integer, String[][]> function)
			throws CalLiteGUIException;

	/**
	 * This will return the full path of the SWP file name which is to load in
	 * the Operations tab.
	 *
	 * @return Will return the full path of the SWP file name which is to load
	 * in the Operations tab.
	 */
	String getWsidiForSWPFullFileName();

	/**
	 * This will set the full path of the SWP file name which is to load in the
	 * Operations tab.
	 *
	 * @param wsidiFileSuffix This is the full path of the SWP file name.
	 */
	void setWsidiForSWPFullFileName(String wsidiFileSuffix);

	/**
	 * This will return the full path of the CVP file name which is to load in
	 * the Operations tab.
	 *
	 * @return Will return the full path of the CVP file name which is to load
	 * in the Operations tab.
	 */
	String getWsidiForCVPFullFileName();

	/**
	 * This will set the full path of the CVP file name which is to load in the
	 * Operations tab.
	 *
	 * @param wsidiForCVPFullFileName This is the full path of the CVP file name.
	 */
	void setWsidiForCVPFullFileName(String wsidiForCVPFullFileName);

	/**
	 * This method is used to get the WSIDI tables.
	 *
	 * @param fileName The whole path of the file with the table name and the
	 *                 extension.
	 * @return Return the Object of {@link DataTableModel} with the table data
	 * in it.
	 * @throws CalLiteGUIException If anything wrong about the file then it will throw a
	 *                             exception with the information about it.
	 */
	DataTableModel getWsiDiTable(String fileName) throws CalLiteGUIException;
}

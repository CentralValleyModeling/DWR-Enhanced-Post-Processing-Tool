/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service;

import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.bo.GUILinks2BO;
import org.swixml.SwingEngine;

/**
 * This is the interface for handling the cls file and run directory for CalLite
 * scenarios.
 *
 * @author Mohan
 */
public interface IScenarioSvc
{

	/**
	 * This will open the cls file and build the following list of strings.
	 *
	 * <pre>
	 * controlStrList
	 * dataTableModelStrList
	 * regulationoptionsStr
	 * </pre>
	 *
	 * @param fileName              The cls file name with complete path.
	 * @param controlStrList        It will take the empty list. When the method is completed this
	 *                              is filled with the control id string from the cls file.
	 * @param dataTableModelStrList It will take the empty list. When the method is completed this
	 *                              is filled with the data table strings from the cls file.
	 * @param regulationoptionsStr  It will take the empty list. When the method is completed this
	 *                              is filled with the regulation options string from the cls
	 *                              file.
	 * @param wsidiStatusStr        List to be filled with WSIDISTATUS entries from cls file
	 */
	void getCLSData(String fileName, List<String> controlStrList, List<String> dataTableModelStrList,
					List<String> regulationoptionsStr, List<String> wsidiStatusStr);

	/**
	 * This will open the cls file read in the data and apply it for the current
	 * ui.
	 *
	 * @param fileName    The cls file name with complete path.
	 * @param swingEngine The object of the GUI.
	 * @param tableMap    The map with key as the table id and value as table object.
	 */
	void applyClsFile(String fileName, SwingEngine swingEngine, Map<String, GUILinks2BO> tableMap);

	/**
	 * This will save the current state of the ui into the cls file and the
	 * Scenario directory.
	 *
	 * @param fileName        The cls file name with out the path and the extension.
	 * @param swingEngine     The object of the GUI.
	 * @param guiLinks2BOList The data list from gui_link2.table.
	 * @return Will return true if the save if done.
	 */
	boolean save(String fileName, SwingEngine swingEngine, List<GUILinks2BO> guiLinks2BOList);

	/**
	 * This will return the Regulation options data.
	 *
	 * @return Will return the Regulation options data.
	 */
	int[] getRegulationoptions();

	/**
	 * This will add the {@code tableName} as key and the {@code dataTableModle}
	 * as value to the user defined table map.
	 *
	 * @param tableName      The table name
	 * @param dataTableModel The object of {@link DataTableModel}.
	 */
	void addUserDefinedTable(String tableName, DataTableModel dataTableModel);

	/**
	 * Will return the user defined table for the given {@code tableName}. if
	 * the table is not there it will return null.
	 *
	 * @param tableName Just table name as per the gui_link2.table.
	 * @return Will return the user defined table for the given
	 * {@code tableName}
	 */
	DataTableModel getUserDefinedTable(String tableName);

	/**
	 * This will tell whether the table name has the user defined table or not.
	 *
	 * @param tableName Just table name as per the gui_link2.table.
	 * @return Will tell whether the table name has the user defined table or
	 * not.
	 */
	boolean hasUserDefinedTable(String tableName);

	/**
	 * It will delete the table if it is in the user defined table map.
	 *
	 * @param tableName Just table name as per the gui_link2.table.
	 */
	void removeUserDefinedTable(String tableName);

	/**
	 * It will return true when the cls file is loading.
	 *
	 * @return Will return true when the cls file is loading.
	 */
	boolean isCLSFileLoading();

	/**
	 * This will save the current ui state to the cls file.
	 *
	 * @param fileName        Just the file name with out the path and extension.
	 * @param swingEngine     The object of the GUI.
	 * @param guiLinks2BOList The list of seed Data.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	void saveToCLSFile(String fileName, SwingEngine swingEngine, List<GUILinks2BO> guiLinks2BOList)
			throws CalLiteGUIException;

	/**
	 * This method will take the string and tell whether it's Double or not.
	 *
	 * @param value The string to be checked.
	 * @return return true if the string is Double.
	 */
	boolean isDouble(String value);
}

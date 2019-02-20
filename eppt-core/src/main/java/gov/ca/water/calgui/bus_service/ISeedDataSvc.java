/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service;

import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.bo.GUILinks2BO;
import gov.ca.water.calgui.bo.GUILinks3BO;
import gov.ca.water.calgui.bo.GUILinks4BO;

/**
 * This is the interface for loading the GUI_Link2.table and GUI_Link4.table.
 *
 * @author Mohan
 */
public interface ISeedDataSvc
{

	/**
	 * This will take the {@code guiId} and return the Object of that
	 * {@code guiId}. If the guiId is not there then it will return null.
	 *
	 * @param guiId The guiId.
	 * @return Will return the GUILinks2 Data object of the gui id passed in.
	 */
	GUILinks2BO getObjByGuiId(String guiId);

	/**
	 * This will concatenate "RunBasis_ID CCProject_ID CCModel_ID" id's from the
	 * Gui_Link4.table and match with the {@code id} and return the object which
	 * is matched. If the {@code id} is not there then it will return null.
	 *
	 * @param id The id value for "RunBasis_ID LOD_ID CCProject_ID CCModel_ID".
	 * @return Will return the gui link object.
	 */
	GUILinks4BO getObjByRunBasisLodCcprojCcmodelIds(String id);

	/**
	 * This will take the {@code id} and return the Object of that {@code guiId}
	 * . If the guiId is not there then it will return null.
	 *
	 * @param id The id.
	 * @return Will return the GUILinks3 Data object for the id passed in.
	 */
	GUILinks3BO getObjById(String id);

	/**
	 * This will return the list of {@link GUILinks2BO} objects which has data
	 * Table value in it.
	 *
	 * @return Will return the list of {@link GUILinks2BO} objects which has
	 * data Table value in it.
	 */
	List<GUILinks2BO> getUserTables();

	/**
	 * This will return the Gui_Link2.table data as list of {@link GUILinks2BO}
	 * Objects.
	 *
	 * @return Will return the Gui_Link2.table data as list of
	 * {@link GUILinks2BO} Objects.
	 */
	List<GUILinks2BO> getGUILinks2BOList();

	/**
	 * This will return the map with key as the Table id and value as
	 * {@link GUILinks2BO}.
	 *
	 * @return Will return the map with key as the Table id and value as
	 * {@link GUILinks2BO}.
	 */
	Map<String, GUILinks2BO> getTableIdMap();

	/**
	 * This will tell whether the guiId have {@link GUILinks2BO} or not.
	 *
	 * @param guiId The gui Id
	 * @return Will tell whether the guiId have {@link GUILinks2BO} or not.
	 */
	boolean hasSeedDataObject(String guiId);

	/**
	 * This will return the list of {@link GUILinks2BO} which belong to the
	 * Regulation Tab.
	 *
	 * @return Will return the list of {@link GUILinks2BO} which belong to the
	 * Regulation Tab.
	 */
	List<GUILinks2BO> getRegulationsTabData();

}

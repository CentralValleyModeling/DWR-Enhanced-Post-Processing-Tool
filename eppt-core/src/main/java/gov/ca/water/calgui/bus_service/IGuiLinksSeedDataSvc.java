/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service;

import gov.ca.water.calgui.bo.GUILinks3BO;

/**
 * This is the interface for loading the GUI_Link2.table and GUI_Link4.table.
 *
 * @author Mohan
 */
public interface IGuiLinksSeedDataSvc
{


	/**
	 * This will take the {@code id} and return the Object of that {@code guiId}
	 * . If the guiId is not there then it will return null.
	 *
	 * @param id The id.
	 * @return Will return the GUILinks3 Data object for the id passed in.
	 */
	GUILinks3BO getObjById(String id);



}

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

package gov.ca.water.calgui.busservice;

import java.util.List;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;

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
	GUILinksAllModelsBO getGuiLink(String id);

	List<GUILinksAllModelsBO> getAllGuiLinks();

}

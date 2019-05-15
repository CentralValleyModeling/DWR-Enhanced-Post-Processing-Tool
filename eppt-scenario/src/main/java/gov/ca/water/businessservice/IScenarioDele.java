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

package gov.ca.water.businessservice;

import java.util.List;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.DataTableModel;
import org.swixml.SwingEngine;

/**
 * This is used to show the Scenario state of the application and also compare
 * to the saved files if provided.
 *
 * @author Mohan
 */
public interface IScenarioDele
{
	/**
	 * This will add the Current_Scenario.CLS file to the file names and then
	 * build the List of {@link DataTableModel}.
	 *
	 * @param fileNames File names that should be displayed in the
	 *                  {@link ScenarioFrame}. Send null if you want to see only the
	 *                  current Scenario in the frame.
	 * @return Will return only one object if the file name is null that is the
	 * Current Scenario. Otherwise it will return three objects in which
	 * the 1st is Current Scenario, 2nd is the comparison and the 3ed is
	 * differences.
	 */
	List<DataTableModel> getScenarioTableData(List<String> fileNames, SwingEngine swingEngine)
			throws EpptInitializationException;
}
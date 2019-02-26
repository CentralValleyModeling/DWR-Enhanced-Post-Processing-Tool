/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_delegate;

import java.util.List;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.presentation.ScenarioFrame;

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
	List<DataTableModel> getScenarioTableData(List<String> fileNames) throws EpptInitializationException;
}
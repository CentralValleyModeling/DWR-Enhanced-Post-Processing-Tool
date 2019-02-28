/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice;

/**
 * This interface will apply the dynamic behaver which is controlled by the
 * files listed bellow.
 *
 * <pre>
 * 	1. TriggerForDymanicSelection.csv
 * 	2. TriggerForDymanicSelection.csv
 * </pre>
 *
 * @author Mohan
 */
public interface IApplyDynamicConDele
{
	/**
	 * This method will apply the dynamic behaver which is controlled by the
	 * files listed bellow and it also handle the special cases for the tabs.
	 *
	 * <pre>
	 * 	1. TriggerForDymanicSelection.csv
	 * 	2. TriggerForDymanicSelection.csv
	 * </pre>
	 *
	 * @param itemName         Name of the item which this method is going to handle.
	 * @param isSelected       whether the item is selected or not.
	 * @param isEnabled        whether the item is enabled or not.
	 * @param optionFromTheBox This is the special field which is used for the popup box
	 *                         result in "run Settings" and "hydroclimate" tabs.
	 */
	void applyDynamicControl(String itemName, boolean isSelected, boolean isEnabled, boolean optionFromTheBox);

	/**
	 * This method will change the sv and init file names and update the table
	 * names for the "Operations" tab.
	 *
	 * @param optionFromTheBox This is the special field which is used for the popup box
	 *                         result in "run Settings" and "hydroclimate" tabs.
	 */
	void changeSVInitFilesAndTableInOperations(boolean optionFromTheBox);

	/**
	 * This method will be executed only at the start or load of the cls file.
	 * It used DYMANIC_CONTROL_FOR_STARTUP_FILENAME file items to apply the
	 * dymanic which are turned on.
	 */
	void applyDynamicControlForListFromFile();
}

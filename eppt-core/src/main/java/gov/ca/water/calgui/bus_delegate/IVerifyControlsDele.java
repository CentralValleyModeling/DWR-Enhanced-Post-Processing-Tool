/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_delegate;

/**
 * This interface is used for verifying the data before the application is
 * started.
 *
 * @author Mohan
 */
public interface IVerifyControlsDele
{

	/**
	 * This method will verify the Gui Id with the seedData and the cls file.
	 *
	 * @param fileName This is the file name with path of the cls file.
	 */
	void verifyTheDataBeforeUI(String fileName);
}

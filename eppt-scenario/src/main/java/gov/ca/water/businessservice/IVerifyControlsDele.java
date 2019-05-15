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

import gov.ca.water.calgui.EpptInitializationException;

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
	void verifyTheDataBeforeUI(String fileName) throws EpptInitializationException;
}

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

package gov.ca.water.scenario;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.scenario.presentation.CalLiteInitClass;
//! Main class


/**
 * This is the root class for this application. We start the application using
 * this class.
 *
 * @author Mohan
 */
public class CalLiteGUI
{
	public static void main(String[] args)
	{
		try
		{
			CalLiteInitClass calLiteInit = new CalLiteInitClass();
			calLiteInit.init();
		}
		catch(EpptInitializationException ex)
		{
			//do something. Issue finding or reading the data
		}
	}
}

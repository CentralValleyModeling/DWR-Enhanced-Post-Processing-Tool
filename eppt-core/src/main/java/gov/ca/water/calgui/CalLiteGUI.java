/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui;

//! Main class

import gov.ca.water.calgui.presentation.CalLiteInitClass;

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
		CalLiteInitClass calLiteInit = new CalLiteInitClass();
		calLiteInit.init();
	}
}

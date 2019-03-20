/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.techservice;

/**
 * Interface for common JOptionPane for simple dialogs with CalLite logo,
 * centered on main frame
 *
 * @author tslawecki
 */
public interface IDialogSvc
{

	String getOK(String message, int messageType);

	String getYesNo(String message, int messageType);

	String getOKCancel(String message, int messageType);

	String getSaveDontSaveCancel(String message, int messageType);

	String getYesNoCancel(String message, int messageType);

}
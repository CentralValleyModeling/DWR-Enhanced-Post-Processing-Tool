/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.tech_service;

import javax.swing.*;

import gov.ca.water.calgui.bo.CalLiteGUIException;

/**
 * This is the interface for handling different type of error like validatione,
 * business, system and also used for displaying the error messages.
 *
 * @author Mohan
 */
public interface IErrorHandlingSvc
{
	/**
	 * This method is used to display the Validation related Errors. For example
	 * when user forget to enter the value in a field then we use this method to
	 * display the error.
	 *
	 * @param displayMessage Message message to display to the user.
	 * @param detailMessage  Detail message with stack trace for additional information.
	 * @param mainFrame      For displaying the message.
	 */
	void validationeErrorHandler(String displayMessage, String detailMessage, JFrame mainFrame);

	/**
	 * This method is used to display the Validation related Errors. For example
	 * when user forget to enter the value in a field then we use this method to
	 * display the error.
	 *
	 * @param mainFrame  For displaying the message.
	 * @param aThrowable An exception class which has all messages in layer and the
	 *                   stack trace.
	 */
	void validationeErrorHandler(JFrame mainFrame, Throwable aThrowable);

	/**
	 * This method is used to display the Business related Errors. For example
	 * when we are doing some computation and if we get an error then we should
	 * use this method to display the error when the file is missing then we can
	 * use to tell the user.
	 *
	 * @param displayMessage Message message to display to the user.
	 * @param detailMessage  Detail message with stack trace for additional information.
	 * @param mainFrame      For displaying the message.
	 */
	void businessErrorHandler(String displayMessage, String detailMessage, JFrame mainFrame);

	/**
	 * This method is used to display the Business related Errors. For example
	 * when we are doing some computation and if we get an error then we should
	 * use this method to display the error when the file is missing then we can
	 * use to tell the user.
	 *
	 * @param mainFrame  For displaying the message.
	 * @param aThrowable An exception class which has all messages in layer and the
	 *                   stack trace.
	 */
	void businessErrorHandler(JFrame mainFrame, Throwable aThrowable);

	/**
	 * We should display this error when the error which is not been able to fix
	 * by the user. This method will close the Application.
	 *
	 * @param displayMessage message to display to the user.
	 * @param detailMessage  Detail message with stack trace for additional information.
	 * @param mainFrame      For displaying the message.
	 */
	void systemErrorHandler(String displayMessage, String detailMessage, JFrame mainFrame);

	/**
	 * We should display this error when the error which is not been able to fix
	 * by the user. This method will close the Application.
	 *
	 * @param mainFrame  For displaying the message.
	 * @param aThrowable An exception class which has all messages in layer and the
	 *                   stack trace.
	 */
	void systemErrorHandler(JFrame mainFrame, Throwable aThrowable);

	/**
	 * This method will take the Throwable and convert the the Stack Trace into
	 * a string and return it.
	 *
	 * @param aThrowable An exception class which has all messages in layer and the
	 *                   stack trace.
	 * @return Will return whole stack trace as string.
	 */
	String getStackTraceAsString(Throwable aThrowable);

	/**
	 * This method is used to display the error message's before the ui is
	 * built.
	 *
	 * @param ex The error message to be displayed.
	 */
	void displayErrorMessageBeforeTheUI(CalLiteGUIException ex);

	/**
	 * @param displayMessage
	 * @param mainFrame
	 * @param aThrowable
	 */
	void businessErrorHandler(String displayMessage, JFrame mainFrame, Throwable aThrowable);


}

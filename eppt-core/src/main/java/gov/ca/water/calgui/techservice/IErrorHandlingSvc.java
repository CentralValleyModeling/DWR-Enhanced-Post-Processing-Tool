/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.techservice;

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
	 */
	void validationeErrorHandler(String displayMessage, String detailMessage);


	/**
	 * This method is used to display the Business related Errors. For example
	 * when we are doing some computation and if we get an error then we should
	 * use this method to display the error when the file is missing then we can
	 * use to tell the user.
	 *
	 * @param displayMessage Message message to display to the user.
	 * @param detailMessage  Detail message with stack trace for additional information.
	 */
	void businessErrorHandler(String displayMessage, String detailMessage);

	/**
	 * This method is used to display the Business related Errors. For example
	 * when we are doing some computation and if we get an error then we should
	 * use this method to display the error when the file is missing then we can
	 * use to tell the user.
	 *
	 * @param aThrowable An exception class which has all messages in layer and the
	 *                   stack trace.
	 */
	void businessErrorHandler(Throwable aThrowable);

	/**
	 * We should display this error when the error which is not been able to fix
	 * by the user. This method will close the Application.
	 *
	 * @param displayMessage message to display to the user.
	 * @param detailMessage  Detail message with stack trace for additional information.
	 */
	void systemErrorHandler(String displayMessage, String detailMessage);

	/**
	 * @param displayMessage
	 * @param aThrowable
	 */
	void businessErrorHandler(String displayMessage, Throwable aThrowable);


}

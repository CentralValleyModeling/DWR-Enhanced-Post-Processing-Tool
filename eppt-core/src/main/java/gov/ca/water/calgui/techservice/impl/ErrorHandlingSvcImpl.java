/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.techservice.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.techservice.IErrorHandlingSvc;

/**
 * This is the class for handling different type of error like validation
 * business, system and also used for displaying the error messages.
 *
 * @author Mohan
 */
public class ErrorHandlingSvcImpl implements IErrorHandlingSvc
{

	private static final Logger LOGGER = Logger.getLogger(ErrorHandlingSvcImpl.class.getName());

	@Override
	public void validationeErrorHandler(String displayMessage, String detailMessage)
	{
		displayErrorMessage("Validation Error : " + displayMessage, detailMessage, null);
	}

	@Override
	public void businessErrorHandler(Throwable aThrowable)
	{
		displayErrorMessage("Business Error : " + aThrowable.getMessage(), null, aThrowable);
	}

	@Override
	public void businessErrorHandler(String displayMessage, Throwable aThrowable)
	{

		displayErrorMessage("Business Error : " + displayMessage, null, aThrowable);
	}

	@Override
	public void businessErrorHandler(String displayMessage, String detailMessage)
	{
		displayErrorMessage("Business Error : " + displayMessage, detailMessage, null);
	}

	@Override
	public void systemErrorHandler(String displayMessage, String detailMessage)
	{
		displayErrorMessage("System Error : " + displayMessage, detailMessage, null);
		System.exit(-1);
	}

	/**
	 * This method will display the message to the user in the JOptionPane and
	 * send email.
	 *
	 * @param displayMessage Message to display the user.
	 * @param detailMessage  Detail message with stack trace for additional information.
	 */
	private void displayErrorMessage(String displayMessage, String detailMessage, Throwable throwable)
	{
		String emailMessage = "Display Message : " + displayMessage;
		if(detailMessage != null)
		{
			emailMessage += "\n" + "Detail Message : " + detailMessage;
		}
		if(throwable == null)
		{
			LOGGER.log(Level.SEVERE, emailMessage);
		}
		else
		{
			LOGGER.log(Level.SEVERE, emailMessage, throwable);
		}
	}
}

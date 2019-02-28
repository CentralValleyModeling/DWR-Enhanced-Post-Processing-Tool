/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.tech_service.impl;

import java.awt.Dimension;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.swing.*;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import org.apache.log4j.Logger;

/**
 * This is the class for handling different type of error like validation
 * business, system and also used for displaying the error messages.
 *
 * @author Mohan
 */
public class ErrorHandlingSvcImpl implements IErrorHandlingSvc
{

	private static final Logger LOG = Logger.getLogger(ErrorHandlingSvcImpl.class.getName());
	private static final Properties properties;
	private static long time = System.currentTimeMillis();

	static
	{
		properties = new Properties();
		try
		{
			properties.load(ErrorHandlingSvcImpl.class.getClassLoader().getResourceAsStream("callite-gui.properties"));
		}
		catch(Exception ex)
		{
			LOG.error("Problem loading properties. " + ex.getMessage(), ex);
		}
	}

	@Override

	public void validationeErrorHandler(String displayMessage, String detailMessage)
	{
		displayErrorMessage("Validation Error : " + displayMessage, detailMessage);
	}

	@Override
	public void businessErrorHandler(Throwable aThrowable)
	{
		List<String> error = getMessageAndStackTraceFromLayeredError(aThrowable);
		displayErrorMessage("Business Error : " + error.get(0), error.get(1));
	}

	@Override
	public void businessErrorHandler(String displayMessage, Throwable aThrowable)
	{
		List<String> error = getMessageAndStackTraceFromLayeredError(aThrowable);
		displayErrorMessage("Business Error : " + displayMessage, error.get(1));
	}

	@Override
	public void businessErrorHandler(String displayMessage, String detailMessage)
	{
		displayErrorMessage("Business Error : " + displayMessage, detailMessage);
	}

	@Override
	public void systemErrorHandler(String displayMessage, String detailMessage)
	{
		displayErrorMessage("System Error : " + displayMessage, detailMessage);
		System.exit(-1);
	}

	@Override
	public String getStackTraceAsString(Throwable aThrowable)
	{
		Writer result = new StringWriter();
		PrintWriter printWriter = new PrintWriter(result);
		try
		{
			aThrowable.printStackTrace(printWriter);
			return result.toString();
		}
		catch(NullPointerException ex)
		{
			return "";
		}
	}


	/**
	 * This method will change the layered exceptions into the message string
	 * and also have the stack trace as a string.
	 *
	 * @param aThrowable An exception class which has all messages in layer and the
	 *                   stack trace.
	 * @return In the list the 1st value will be the error message and the 2nd
	 * value will be the stack trace of the {@link Exception}.
	 */
	private List<String> getMessageAndStackTraceFromLayeredError(Throwable aThrowable)
	{
		StringBuilder errorMessage = new StringBuilder();
		List<String> list = new ArrayList<>();
		String stackTrace = "";
		while(true)
		{
			if(aThrowable instanceof CalLiteGUIException)
			{
				errorMessage.append(aThrowable.getMessage() + Constant.NEW_LINE);
				aThrowable = aThrowable.getCause();
			}
			else
			{
				stackTrace = this.getStackTraceAsString(aThrowable);
				break;
			}
		}
		list.add(errorMessage.toString());
		list.add(stackTrace);
		return list;
	}

	/**
	 * This method will display the message to the user in the JOptionPane and
	 * send email.
	 *
	 * @param displayMessage Message to display the user.
	 * @param detailMessage  Detail message with stack trace for additional information.
	 */
	private void displayErrorMessage(String displayMessage, String detailMessage)
	{
		if((System.currentTimeMillis() - ErrorHandlingSvcImpl.time) < 2000)
		{
			return;
		}
		ImageIcon icon = new ImageIcon(getClass().getResource("/images/CalLiteIcon.png"));
		ErrorHandlingSvcImpl.time = System.currentTimeMillis();
		Object[] options = {"ok", "show details"};
		JOptionPane optionPane = new JOptionPane(displayMessage, JOptionPane.ERROR_MESSAGE, JOptionPane.YES_NO_OPTION,
				null, options, options[1]);
		JDialog dialog = optionPane.createDialog(null, "CalLite GUI");
		dialog.setIconImage(icon.getImage());
		dialog.setResizable(false);
		dialog.setVisible(true);
		String emailMessage = "Display Message : " + displayMessage + "\n" + "Detail Message : " + detailMessage;
		JTextArea text = new JTextArea(detailMessage);
		JScrollPane scroll = new JScrollPane(text);
		scroll.setPreferredSize(new Dimension(600, 400));
		if("show details".equals(optionPane.getValue().toString()))
		{
			JOptionPane.showMessageDialog(null, scroll, "Error", JOptionPane.ERROR_MESSAGE, icon);
		}
		LOG.debug(emailMessage);
		sendEmail(emailMessage);
	}

	/**
	 * This method will send email.
	 *
	 * @param message   The detail message which is send in the email.
	 */
	private void sendEmail(String message)
	{
		if(Boolean.parseBoolean(properties.getProperty("email.to.developer")))
		{
			Properties props = new Properties();
			props.put("mail.smtp.host", "smtp.gmail.com");
			props.put("mail.transport.protocol", "smtp");
			props.put("mail.smtp.starttls.enable", "true");

			Session session = Session.getDefaultInstance(props);

			try
			{
				String userName = properties.getProperty("user.name");
				InternetAddress fromAddress = new InternetAddress(userName + "@gmail.com");
				InternetAddress toAddress = new InternetAddress(properties.getProperty("to.address"));

				Message mes = new MimeMessage(session);
				mes.setFrom(fromAddress);
				mes.setRecipient(Message.RecipientType.TO, toAddress);
				mes.setSubject(properties.getProperty("subject"));
				mes.setText(message);
				Transport.send(mes, userName, properties.getProperty("password"));
			}
			catch(MessagingException ex)
			{
				LOG.error(getStackTraceAsString(ex));
				JOptionPane.showMessageDialog(null, "Can't send email to the developer.", "Error",
						JOptionPane.ERROR_MESSAGE);
			}
		}
	}
}

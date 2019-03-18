/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.gui;

/**
 * Exception super class for all exceptions thrown by all commands.
 * 
 * @author Nicky Sandhu
 * @version $Id: ExecutionException.java,v 1.1 2003/10/02 20:49:14 redwood Exp $
 */
public class ExecutionException extends Exception {
	/**
   *
   */
	public ExecutionException(String msg) {
		super(msg);
	}

	/**
	 * Displays error
	 */
	public ExecutionException(Command c) {
		this("Error executing command: " + c.getClass().getName());
	}

	/**
   *
   */
	public ExecutionException(Exception e, String msg) {
		this(msg);
		_ex = e;
	}

	/**
	 * writes out nested exception message and its own message
	 */
	public String getMessage() {
		String msg;
		if (_ex != null) {
			msg = "Nested Exception: " + _ex.toString();
			msg += super.getMessage();
		} else
			msg = super.getMessage();
		return msg;
	}

	/**
   *
   */
	public void printStackTrace(java.io.PrintWriter s) {
		if (_ex != null)
			_ex.printStackTrace(s);
		super.printStackTrace(s);
	}

	/**
   *
   */
	public void printStackTrace(java.io.PrintStream s) {
		if (_ex != null)
			_ex.printStackTrace(s);
		super.printStackTrace(s);
	}

	private Exception _ex;
}

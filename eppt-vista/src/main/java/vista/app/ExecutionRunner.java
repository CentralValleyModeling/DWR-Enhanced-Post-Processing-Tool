/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import vista.gui.Command;

/**
 * Executes the command in the background
 * 
 * @author Nicky Sandhu
 * @version $Id: ExecutionRunner.java,v 1.1 2003/10/02 20:48:28 redwood Exp $
 */
public class ExecutionRunner implements Runnable {
	private Command _com;
	private View _view;

	/**
	 * initializes the runner with the command and the affected view
	 */
	public ExecutionRunner(Command com, View view) {
		_com = com;
		_view = view;
	}

	/**
	 * run method
	 */
	public void run() {
		Executor.execute(_com, _view);
	}
}

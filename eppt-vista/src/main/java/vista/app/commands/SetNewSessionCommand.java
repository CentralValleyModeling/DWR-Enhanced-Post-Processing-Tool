/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.commands;

import vista.app.SessionContext;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: SetNewSessionCommand.java,v 1.1 2003/10/02 20:48:41 redwood Exp
 *          $
 */
public class SetNewSessionCommand implements Command {
	private SessionContext _app;
	private Session _previousSession;

	/**
	 * SetNews session and sets current session to
	 */
	public SetNewSessionCommand(SessionContext app) {
		_app = app;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		Session s = new Session();
		_previousSession = _app.getCurrentSession();
		_app.setCurrentSession(s);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		_app.setCurrentSession(_previousSession);
	}

	/**
	 * checks if command is executable.
	 */
	public boolean isUnexecutable() {
		return true;
	}

	/**
	 * writes to script
	 */
	public void toScript(StringBuffer buf) {
	}
} // end of SetNewSessionCommand

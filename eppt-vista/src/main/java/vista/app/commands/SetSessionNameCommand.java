/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.commands;

import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.gui.VistaException;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: SetSessionNameCommand.java,v 1.1 2003/10/02 20:48:41 redwood
 *          Exp $
 */
public class SetSessionNameCommand implements Command {
	private Session _session;
	private String _name, _previousName;

	/**
	 * SetNews session and sets current session to
	 */
	public SetSessionNameCommand(Session s, String name) {
		_session = s;
		_name = name;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_name == null)
			throw new VistaException("session name is null");
		if (_previousName == null)
			_previousName = _session.getName();
		_session.setName(_name);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		if (_previousName == null)
			return;
		_session.setName(_previousName);
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

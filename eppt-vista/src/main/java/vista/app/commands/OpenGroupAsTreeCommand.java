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
import vista.set.Group;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: OpenGroupAsTreeCommand.java,v 1.2 1998/10/13 16:28:20 nsandhu
 *          Exp $
 */
public class OpenGroupAsTreeCommand implements Command {
	private SessionContext _context;
	private Session _session;
	private int[] _gNumbers;
	private Group _group;

	/**
	 * opens session and sets current session to
	 */
	public OpenGroupAsTreeCommand(SessionContext context, Session s,
			int[] groupNumbers) {
		_context = context;
		_session = s;
		_gNumbers = groupNumbers;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_gNumbers == null || _gNumbers.length == 0)
			return;
		for (int i = 0; i < _gNumbers.length; i++) {
			if (_group == null)
				_group = _session.getGroup(_gNumbers[i]);
			else
				_group = _group.unionWith(_session.getGroup(_gNumbers[i]));
		}
		_context.setCurrentGroup(_group);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
	}

	/**
	 * checks if command is executable.
	 */
	public boolean isUnexecutable() {
		return false;
	}

	/**
	 * writes to script
	 */
	public void toScript(StringBuffer buf) {
	}
} // end of Open GroupCommand

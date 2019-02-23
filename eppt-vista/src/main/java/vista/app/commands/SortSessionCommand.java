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
import vista.set.SortMechanism;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: SortSessionCommand.java,v 1.1 2003/10/02 20:48:42 redwood Exp $
 */
public class SortSessionCommand implements Command {
	private SessionContext _sc;
	private Session _session, _previousSession;
	private SortMechanism<Group> _sorter;

	/**
	 * opens session and sets current session to
	 */
	public SortSessionCommand(SessionContext sc, Session s, SortMechanism<Group> sortMechanism) {
		_session = s;
		_sorter = sortMechanism;
		_sc = sc;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		_previousSession = (Session) _session.clone();
		_session.sortBy(_sorter);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		_session = _previousSession;
		if (_previousSession != null)
			_sc.setCurrentSession(_previousSession);
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
} // end of SortSessionCommand

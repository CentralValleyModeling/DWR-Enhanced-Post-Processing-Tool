/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.commands;

import java.util.ArrayList;
import java.util.Iterator;

import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Group;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: RemoveGroupCommand.java,v 1.1 2003/10/02 20:48:39 redwood Exp $
 */
public class RemoveGroupCommand implements Command {
	private Session _session;
	private int[] _gNumbers;
	private ArrayList<Object> _gRemoved;

	/**
	 * opens session and sets current session to
	 */
	public RemoveGroupCommand(Session s, int[] groupNumbers) {
		_session = s;
		_gNumbers = groupNumbers;
		_gRemoved = new ArrayList<Object>();
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_gNumbers == null || _gNumbers.length == 0)
			throw new IllegalArgumentException(
					"No references selected for deletion");
		// first get all the objects to be removed...
		for (int i = 0; i < _gNumbers.length; i++) {
			int ng = _gNumbers[i];
			_gRemoved.add(new Integer(ng));
			_gRemoved.add(_session.getGroup(ng));
		}
		// then remove them
		for (Iterator<Object> eg = _gRemoved.iterator(); eg.hasNext();) {
			eg.next();
			_session.removeGroup((Group) eg.next());
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		int n = _gRemoved.size();
		if (n == 0)
			return;
		for (Iterator<Object> eg = _gRemoved.iterator(); eg.hasNext();) {
			int gNumber = ((Integer) eg.next()).intValue();
			Group group = (Group) eg.next();
			_session.insertGroupAt(gNumber, group);
		}
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
} // end of RemoveGroupCommand

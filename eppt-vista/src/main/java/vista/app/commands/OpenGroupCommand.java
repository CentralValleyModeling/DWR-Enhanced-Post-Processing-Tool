/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
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
 * @version $Id: OpenGroupCommand.java,v 1.1 2003/10/02 20:48:36 redwood Exp $
 */
public class OpenGroupCommand implements Command {
	private SessionContext _context;
	private Session _session;
	private int[] _gNumbers;
	private Group _group;

	/**
	 * opens session and sets current session to
	 */
	public OpenGroupCommand(SessionContext context, Session s,
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

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

import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Group;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 *
 * @author Nicky Sandhu
 * @version $Id: MoveGroupCommand.java,v 1.1 2003/10/02 20:48:34 redwood Exp $
 */
public class MoveGroupCommand implements Command
{
	private Session _session;
	private int _oPos, _nPos;

	/**
	 * opens session and sets current session to
	 */
	public MoveGroupCommand(Session s, int oldPosition, int newPosition)
	{
		_session = s;
		_oPos = oldPosition;
		_nPos = newPosition;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		Group g = _session.getGroup(_oPos);
		_session.removeGroup(g);
		_session.insertGroupAt(_nPos, g);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
		Group g = _session.getGroup(_nPos);
		_session.removeGroup(g);
		_session.insertGroupAt(_oPos, g);
	}

	/**
	 * checks if command is executable.
	 */
	public boolean isUnexecutable()
	{
		return true;
	}

	/**
	 * writes to script
	 */
	public void toScript(StringBuffer buf)
	{
	}
} // end of MoveGroupCommand

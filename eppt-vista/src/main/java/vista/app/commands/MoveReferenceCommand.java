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
import vista.set.DataReference;
import vista.set.Group;

/**
 * Encapsulates commands implementing session related commands
 *
 * @author Nicky Sandhu
 * @version $Id: MoveReferenceCommand.java,v 1.1 2003/10/02 20:48:35 redwood Exp
 * $
 */
public class MoveReferenceCommand implements Command
{
	private Group _group;
	private int _oPos, _nPos;

	/**
	 * opens session and sets current session to
	 */
	public MoveReferenceCommand(Group g, int oldPosition, int newPosition)
	{
		_group = g;
		_oPos = oldPosition;
		_nPos = newPosition;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		DataReference ref = _group.getDataReference(_oPos);
		_group.removeDataReference(ref);
		_group.insertDataReferenceAt(_nPos, ref);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
		DataReference ref = _group.getDataReference(_nPos);
		_group.removeDataReference(ref);
		_group.insertDataReferenceAt(_oPos, ref);
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
} // end of MoveReferenceCommand

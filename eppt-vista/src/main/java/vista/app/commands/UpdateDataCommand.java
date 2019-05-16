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

import vista.db.dss.DSSUtil;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.DataReference;

/**
 * Encapsulates commands implementing session related commands
 *
 * @author Nicky Sandhu
 * @version $Id: UpdateDataCommand.java,v 1.1 2003/10/02 20:48:43 redwood Exp $
 */
public class UpdateDataCommand implements Command
{
	private DataReference[] _refs;

	/**
	 * opens session and sets current session to
	 */
	public UpdateDataCommand(DataReference[] refs)
	{
		_refs = refs;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		synchronized(_refs)
		{
			for(int i = 0; i < _refs.length; i++)
			{
				try
				{
					DSSUtil.updateData(_refs[i], true);
				}
				catch(Exception e)
				{
					throw new RuntimeException(e.getMessage());
				}
			}
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
		throw new ExecutionException("Cannot undo update");
	}

	/**
	 * checks if command is executable.
	 */
	public boolean isUnexecutable()
	{
		return false;
	}

	/**
	 * writes to script
	 */
	public void toScript(StringBuffer buf)
	{
	}
} // end of Open GroupCommand

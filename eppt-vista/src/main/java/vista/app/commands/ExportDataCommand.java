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

import java.io.IOException;

import vista.db.dss.DSSUtil;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.DataReference;
import vista.set.Group;

/**
 * Encapsulates commands implementing group related commands
 *
 * @author Nicky Sandhu
 * @version $Id: ExportDataCommand.java,v 1.1 2003/10/02 20:48:29 redwood Exp $
 */
public class ExportDataCommand implements Command
{
	private Group _group;
	private int[] _rNumbers;
	private String _filename;

	/**
	 * opens group and sets current group to
	 */
	public ExportDataCommand(Group g, int[] numbers, String filename)
	{
		_group = g;
		_rNumbers = numbers;
		_filename = filename;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		if(_filename == null)
		{
			return;
		}
		if(_rNumbers == null || _rNumbers.length == 0)
		{
			return;
		}
		DataReference[] refs = new DataReference[_rNumbers.length];
		for(int i = 0; i < _rNumbers.length; i++)
		{
			refs[i] = _group.getDataReference(_rNumbers[i]);
		}
		try
		{
			DSSUtil.writeText(refs, _filename + ".dss", _filename);
		}
		catch(IOException ioe)
		{
			throw new ExecutionException(ioe, "exception exporting data");
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
		throw new ExecutionException("Cannot undo write to data base");
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
} // end of ExportDataCommand

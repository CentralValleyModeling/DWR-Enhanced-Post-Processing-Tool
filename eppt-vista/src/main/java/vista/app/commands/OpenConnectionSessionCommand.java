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

/**
 * Encapsulates commands implementing session related commands
 *
 * @author Nicky Sandhu
 * @version $Id: OpenConnectionSessionCommand.java,v 1.1 2003/10/02 20:48:36
 * redwood Exp $
 */
public class OpenConnectionSessionCommand implements Command
{
	private SessionContext _app;

	/**
	 * opens session and sets current session to
	 */
	public OpenConnectionSessionCommand(SessionContext app)
	{
		_app = app;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		throw new UnsupportedOperationException("BDAT Oracle connection no longer supported");
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
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
} // end of OpenConnectionSessionCommand

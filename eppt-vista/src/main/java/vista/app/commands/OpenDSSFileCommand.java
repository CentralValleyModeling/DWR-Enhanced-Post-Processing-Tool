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

import java.io.File;

import vista.app.SessionContext;
import vista.app.SessionView;
import vista.db.dss.DSSUtil;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Group;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 *
 * @author Nicky Sandhu
 * @version $Id: OpenDSSFileCommand.java,v 1.1 2003/10/02 20:48:36 redwood Exp $
 */
public class OpenDSSFileCommand implements Command
{
	private String _filename;
	private SessionContext _app;
	private Session _previousSession;
	private Group _previousGroup;
	private SessionView _sessionView;

	/**
	 * opens session and sets current session to
	 */
	public OpenDSSFileCommand(SessionContext sc, String filename, SessionView sv)
	{
		_app = sc;
		if(filename == null)
		{
			throw new IllegalArgumentException("Filename not supplied");
		}
		_filename = filename;
		_sessionView = sv;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		File f = new File(_filename);
		if(!f.exists())
		{
			throw new IllegalArgumentException("No file: " + _filename
					+ " found");
		}
		if(f.isDirectory())
		{
			throw new IllegalArgumentException(_filename + " is a directory");
		}
		String directory = f.getParent();
		Group g = DSSUtil.createGroup("local", _filename);
		Session s = new Session();
		s.addGroup(g);
		_previousSession = _app.getCurrentSession();
		_app.setCurrentSession(s.createUnion(_previousSession));
		// open group in group manager
		_previousGroup = _app.getCurrentGroup();
		_app.setCurrentGroup(g);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
		_app.setCurrentSession(_previousSession);
		_app.setCurrentGroup(_previousGroup);
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
} // end of OpenConnectionSessionCommand

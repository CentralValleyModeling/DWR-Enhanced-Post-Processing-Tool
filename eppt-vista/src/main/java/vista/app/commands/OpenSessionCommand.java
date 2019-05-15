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

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.zip.GZIPInputStream;

import vista.app.SessionContext;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: OpenSessionCommand.java,v 1.1 2003/10/02 20:48:37 redwood Exp $
 */
public class OpenSessionCommand implements Command {
	private String _filename;
	private SessionContext _app;
	private boolean _addOn;
	private Session _previousSession;

	/**
	 * opens session and sets current session to
	 */
	public OpenSessionCommand(SessionContext app, String loadfilename,
			boolean addOn) {
		_app = app;
		_filename = loadfilename;
		_addOn = addOn;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		try {
			if (_filename == null)
				return;
			FileInputStream istream = new FileInputStream(_filename);
			ObjectInputStream pin = new ObjectInputStream(new GZIPInputStream(
					istream));
			Session s = (Session) pin.readObject();
			_previousSession = _app.getCurrentSession();
			if (_addOn)
				_app.setCurrentSession(_previousSession.createUnion(s));
			else
				_app.setCurrentSession(s);
		} catch (FileNotFoundException fnfe) {
			System.err.println(fnfe.getMessage());
			throw new ExecutionException(getClass().getName() + ": File "
					+ _filename + " not found");
		} catch (IOException ioe) {
			System.err.println(ioe.getMessage());
			throw new ExecutionException(getClass().getName()
					+ ": IO Exception while reading " + _filename);
		} catch (ClassNotFoundException cnfe) {
			System.err.println(cnfe.getMessage());
			throw new ExecutionException(getClass().getName() + ":"
					+ cnfe.getMessage() + " not found ");
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		_app.setCurrentSession(_previousSession);
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
} // end of OpenSessionCommand

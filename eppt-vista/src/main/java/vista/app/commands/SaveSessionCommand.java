/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.commands;

import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.gui.VistaUtils;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: SaveSessionCommand.java,v 1.1 2003/10/02 20:48:40 redwood Exp $
 */
public class SaveSessionCommand implements Command {
	private Session _session;
	private String _filename, _oldFilename;
	private boolean _askAlways;

	/**
	 * saves session to file
	 */
	public SaveSessionCommand(Session s, boolean askAlways) {
		_session = s;
		_askAlways = askAlways;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		try {
			_oldFilename = _session.getFilename();
			if (_askAlways || _oldFilename == null) {
				_filename = VistaUtils.getFilenameFromDialog(null,
						java.awt.FileDialog.SAVE, "ssn", "Session Files");
				if (_filename == null)
					return;
				synchronized (_session) {
					_session.saveTo(_filename);
				}
			} else {
				synchronized (_session) {
					_session.save();
				}
			}
		} catch (Exception fnfe) {
			fnfe.printStackTrace(System.err);
			throw new ExecutionException(this);
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		throw new ExecutionException(this);
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
} // end of SaveSessionCommand

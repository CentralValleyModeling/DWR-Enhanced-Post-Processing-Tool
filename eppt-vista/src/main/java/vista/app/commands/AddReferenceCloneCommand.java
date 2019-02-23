/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.commands;

import vista.app.MainGUI;
import vista.app.SessionContext;
import vista.app.SymbolTable;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.DataReference;
import vista.set.Group;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: AddReferenceCloneCommand.java,v 1.1 2003/10/02 20:48:24 redwood
 *          Exp $
 */
public class AddReferenceCloneCommand implements Command {
	private SessionContext _context;
	private DataReference[] _refs;

	/**
	 * opens session and sets current session to
	 */
	public AddReferenceCloneCommand(SessionContext context, DataReference[] refs) {
		_context = context;
		_refs = refs;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		Group g = _context.getCurrentGroup();
		if (_refs == null || g == null)
			return;
		for (int i = 0; i < _refs.length; i++) {
			_refs[i] = DataReference.create(_refs[i]);
			g.addDataReference(_refs[i]);
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		Group g = _context.getCurrentGroup();
		if (_refs == null || g == null)
			return;
		for (int i = 0; i < _refs.length; i++) {
			g.removeDataReference(_refs[i]);
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
		SymbolTable st = MainGUI.getSymbolTable();
		buf.append("for ref in ").append(st.getNameFor(_refs)).append(" :");
		buf.append(" ").append(st.getNameFor(_context.getCurrentGroup()))
				.append(".addDataReference(DataReference.create(ref))");
	}
} // end of Open GroupCommand

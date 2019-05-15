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

import vista.app.MainGUI;
import vista.app.SymbolTable;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Group;
import vista.set.Session;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: AddNewGroupCommand.java,v 1.1 2003/10/02 20:48:23 redwood Exp $
 */
public class AddNewGroupCommand implements Command {
	private Session _session;
	private Group _gnew;

	/**
	 * opens session and sets current session to
	 */
	public AddNewGroupCommand(Session s) {
		_session = s;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		_session.addGroup(_gnew = new Group());
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		_session.removeGroup(_gnew);
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
		String ls = System.getProperty("line.separator");
		buf.append(st.getNameFor(_gnew)).append("= new Group()").append(ls);
		buf.append(st.getNameFor(_session)).append(".addGroup(").append(
				st.getNameFor(_gnew)).append(")").append(ls);
	}
} // end of AddNewGroupCommand

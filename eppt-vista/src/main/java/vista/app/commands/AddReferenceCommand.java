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
 * @version $Id: AddReferenceCommand.java,v 1.1 2003/10/02 20:48:24 redwood Exp
 *          $
 */
public class AddReferenceCommand implements Command {
	private SessionContext _context;
	private DataReference _ref;

	/**
	 * opens session and sets current session to
	 */
	public AddReferenceCommand(SessionContext context, DataReference ref) {
		_context = context;
		_ref = ref;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		Group g = _context.getCurrentGroup();
		if (_ref == null || g == null)
			return;
		g.addDataReference(_ref);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		Group g = _context.getCurrentGroup();
		if (_ref == null || g == null)
			return;
		g.removeDataReference(_ref);
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
		buf.append(st.getNameFor(_context.getCurrentGroup())).append(
				".addDataReference(").append(st.getNameFor(_ref)).append(")");
	}
} // end of Open GroupCommand

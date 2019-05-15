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
import vista.set.Group;
import vista.set.SortMechanism;

/**
 * Encapsulates commands implementing group related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: SortReferencesCommand.java,v 1.1 2003/10/02 20:48:42 redwood
 *          Exp $
 */
public class SortReferencesCommand implements Command {
	private Group _group;
	private SortMechanism _sm;

	/**
	 * opens group and sets current group to
	 */
	public SortReferencesCommand(Group g, SortMechanism sm) {
		_group = g;
		_sm = sm;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_group == null || _sm == null)
			return;
		_group.sortBy(_sm);
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		throw new ExecutionException("Unsorting is expensive");
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
} // end of CloneReferenceCommand

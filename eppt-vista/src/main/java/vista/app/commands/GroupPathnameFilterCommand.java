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
import vista.set.PathnamePredicate;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupPathnameFilterCommand.java,v 1.1 2003/10/02 20:48:31
 *          redwood Exp $
 */
public class GroupPathnameFilterCommand implements Command {
	private Group _group, _originalGroup;
	private String _regExp;
	private boolean _selecting;

	/**
	 * opens session and sets current session to
	 */
	public GroupPathnameFilterCommand(Group g, String regExp, boolean selecting) {
		_group = g;
		_regExp = regExp;
		_selecting = selecting;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_group == null)
			throw new ExecutionException("Group is null");
		else if (_regExp == null)
			throw new ExecutionException("No regular expression");
		else
			;
		//
		_originalGroup = Group.createGroup(_group);
		String modifierName = "";
		// filter by pathnames
		if (!_regExp.equals("")) {
			_group.filterBy(new PathnamePredicate(_regExp), _selecting);
			modifierName = "< Pathname = " + _regExp + " >";
		}
		// group name
		String groupName = _group.getName();
		_group.setName(groupName + "(modified:" + modifierName + ")");
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		if (_group == null || _originalGroup == null)
			return;
		int nrefs = _group.getNumberOfDataReferences();
		if (nrefs > 0)
			_group.removeDataReference(0, nrefs - 1);
		_originalGroup.copyInto(_group);
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
} // end of Open GroupCommand

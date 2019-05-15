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
import vista.set.DataReference;
import vista.set.Group;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: DeleteReferencesCommand.java,v 1.1 2003/10/02 20:48:28 redwood
 *          Exp $
 */
public class DeleteReferencesCommand implements Command {
	private Group _group;
	private int[] _numbers;
	private DataReference[] _removedReferences;

	/**
	 * opens session and sets current session to
	 */
	public DeleteReferencesCommand(Group g, int[] numbers) {
		_group = g;
		_numbers = numbers;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_group == null)
			throw new ExecutionException("Group is null");
		else if (_numbers == null || _numbers.length == 0)
			throw new ExecutionException("No references selected");
		else
			;
		_removedReferences = new DataReference[_numbers.length];
		for (int i = 0; i < _numbers.length; i++) {
			_removedReferences[i] = _group.getDataReference(_numbers[i]);
		}
		for (int i = 0; i < _numbers.length; i++) {
			_group.removeDataReference(_removedReferences[i]);
		}
		String groupName = _group.getName();
		if (groupName.indexOf("(modified: deletion)") < 0)
			_group.setName(groupName + "(modified: deletion)");
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		if (_group == null)
			throw new ExecutionException("Group is null");
		else if (_numbers == null || _numbers.length == 0)
			throw new ExecutionException("No references selected");
		else
			;
		if (_removedReferences == null)
			return; // not executed yet
		for (int i = 0; i < _removedReferences.length; i++) {
			_group.insertDataReferenceAt(_numbers[i], _removedReferences[i]);
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
	}
} // end of Open GroupCommand

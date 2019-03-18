/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import vista.db.dss.DSSUtil;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.Group;
import vista.set.PathPartPredicate;
import vista.set.Pathname;
import vista.set.TimeWindowFilter;
import vista.time.TimeWindow;

/**
 * Encapsulates commands implementing session related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupFilterCommand.java,v 1.1 2003/10/02 20:48:31 redwood Exp $
 */
class GroupFilterCommand implements Command {
	private Group _group, _originalGroup;
	private String[] _regExp;
	private boolean _selecting;

	/**
	 * opens session and sets current session to
	 */
	public GroupFilterCommand(Group g, String[] regExp, boolean selecting) {
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
		else if (_regExp == null || _regExp.length != 6)
			throw new ExecutionException("No regular expression entered");
		else
			;
		//
		_originalGroup = Group.createGroup(_group);
		String modifierName = "";
		// filter by pathnames
		for (int i = 0; i < _regExp.length; i++) {
			if (_regExp[i].equals(""))
				continue;
			modifierName = "< " + Pathname.getPartName(i) + " = " + _regExp[i]
					+ ">";
			if (i == Pathname.D_PART) {
				TimeWindow window = DSSUtil.getTimeFactory().createTimeWindow(
						_regExp[i], "ddMMMyyyy");
				if (window != null)
					_group.filterBy(new TimeWindowFilter(window), _selecting);
			} else
				_group.filterBy(new PathPartPredicate(_regExp[i], i),
						_selecting);
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

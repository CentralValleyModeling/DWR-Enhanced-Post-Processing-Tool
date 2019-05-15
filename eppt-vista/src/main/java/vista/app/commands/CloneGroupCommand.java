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

import java.util.ArrayList;

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
 * @version $Id: CloneGroupCommand.java,v 1.1 2003/10/02 20:48:25 redwood Exp $
 */
public class CloneGroupCommand implements Command
{
	private Session _session;
	private int[] _gNumbers;
	private Group _group;
	private ArrayList<Group> _gclones;

	/**
	 * opens session and sets current session to
	 */
	public CloneGroupCommand(Session s, int[] groupNumbers)
	{
		_session = s;
		_gNumbers = groupNumbers;
		_gclones = new ArrayList<Group>();
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		if(_gNumbers == null || _gNumbers.length == 0)
		{
			return;
		}
		for(int i = 0; i < _gNumbers.length; i++)
		{
			Group clone = (Group) _session.getGroup(_gNumbers[i]).clone();
			_session.addGroup(clone);
			_gclones.add(clone);
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
		int ng = _gclones.size();
		for(Group g : _gclones)
		{
			_session.removeGroup(g);
		}
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
		SymbolTable st = MainGUI.getSymbolTable();
		buf.append(st.getNameFor(_gNumbers)).append("=").append("[");
		buf.append(_gNumbers[0]);
		for(int i = 1; i < _gNumbers.length; i++)
		{
			buf.append(", ");
			buf.append(_gNumbers[i]);
		}
		buf.append("]");
		buf.append("for i in ").append(st.getNameFor(_gNumbers));
		// buf.append(st.getNameFor());
	}
} // end of CloneGroupCommand

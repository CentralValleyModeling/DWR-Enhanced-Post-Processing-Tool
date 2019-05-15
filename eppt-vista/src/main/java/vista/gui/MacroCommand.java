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
package vista.gui;

import java.util.ArrayList;
import java.util.ListIterator;

/**
 * A macro command is a command which executes a list of commands in a certain
 * order as defined by the order of addition.
 * 
 * @see Command
 * @author Nicky Sandhu
 * @version $Id: MacroCommand.java,v 1.1.1.1 1998/09/30 03:59:05 nsandhu Exp $
 */
public class MacroCommand implements Command {
	/**
	 * constructor private
	 */
	private MacroCommand() {
		_list = new ArrayList<Command>();
	}

	/**
	 * adds command to list
	 */
	public void addCommand(Command c) {
		_list.add(c);
	}

	/**
	 * removes command from list
	 */
	public void removeCommand(Command c) {
		_list.remove(c);
	}

	/**
	 * executes the list of commands in order of addition
	 */
	public void execute() throws ExecutionException {
		for(Command c: _list){
			c.execute();
		}
	}

	/**
	 * executes the list of commands in order of addition
	 */
	public void unexecute() throws ExecutionException {
		ListIterator<Command> iter = _list.listIterator(_list.size());
		while(iter.hasPrevious()){
			iter.previous().unexecute();
		}
	}

	/**
   *
   */
	public boolean isUnexecutable() {
		ListIterator<Command> iter = _list.listIterator(_list.size());
		while(iter.hasPrevious()){
			if (!iter.previous().isUnexecutable()){
				return false;
			}
		}
		return true;
	}

	/**
	 * list of commands held in array.
	 */
	private ArrayList<Command> _list;
}

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

/**
 * The command interface for uniformizing access to application functionality.
 * This can be thought of as the common meeting ground for both script and gui
 * access to application functionality.
 *
 * @author Nicky Sandhu
 * @version $Id: Command.java,v 1.1 2003/10/02 20:49:13 redwood Exp $
 */
public interface Command
{
	/**
	 * executes command
	 */
	void execute() throws ExecutionException;

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	void unexecute() throws ExecutionException;

	/**
	 * checks if command is executable.
	 */
	boolean isUnexecutable();
}

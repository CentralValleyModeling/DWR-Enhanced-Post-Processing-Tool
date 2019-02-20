/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

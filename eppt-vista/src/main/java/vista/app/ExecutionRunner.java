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
package vista.app;

import vista.gui.Command;

/**
 * Executes the command in the background
 *
 * @author Nicky Sandhu
 * @version $Id: ExecutionRunner.java,v 1.1 2003/10/02 20:48:28 redwood Exp $
 */
public class ExecutionRunner implements Runnable
{
	private Command _com;
	private View _view;

	/**
	 * initializes the runner with the command and the affected view
	 */
	public ExecutionRunner(Command com, View view)
	{
		_com = com;
		_view = view;
	}

	/**
	 * run method
	 */
	public void run()
	{
		Executor.execute(_com, _view);
	}
}

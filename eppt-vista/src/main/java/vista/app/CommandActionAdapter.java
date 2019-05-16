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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import vista.gui.Command;
import vista.gui.VistaException;

/**
 * A action listener which uses Command interface to execute
 *
 * @author Nicky Sandhu
 * @version $Id: CommandActionAdapter.java,v 1.1.1.1 1998/09/30 03:57:45 nsandhu
 * Exp $
 */
public class CommandActionAdapter implements ActionListener, Runnable
{
	private Command _command;
	private View _view;
	private boolean _execute;
	private boolean _backgroundExecution;

	/**
	 * Exceutes or unexecutes a command depending upon whether execute is true
	 * or false
	 */
	public CommandActionAdapter(Command com, View view, boolean execute,
								boolean backgroundExecution)
	{
		_command = com;
		_view = view;
		_execute = execute;
		_backgroundExecution = backgroundExecution;
	}

	/**
	 * Invoked when an action occurs.
	 */
	public void actionPerformed(ActionEvent evt)
	{
		if(_backgroundExecution)
		{
			new Thread(this).start();
		}
		else
		{
			doAction();
		}
	}

	/**
	 * runs in a thread
	 */
	public void run()
	{
		doAction();
	}

	/**
	 * does the main action of calling the command interface execute or
	 * unexecute method
	 */
	public void doAction()
	{
		try
		{
			if(_execute)
			{
				_command.execute();
			}
			else
			{
				_command.unexecute();
			}
			_view.updateView();
		}
		catch(Exception e)
		{
			throw new VistaException(e, getClass().getName()
					+ " execution problem");
		}
	}
}

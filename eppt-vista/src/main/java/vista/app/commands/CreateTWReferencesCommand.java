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

import java.util.StringTokenizer;
import javax.swing.*;

import vista.db.dss.DSSUtil;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.DataReference;
import vista.set.Group;
import vista.time.Time;
import vista.time.TimeFactory;
import vista.time.TimeWindow;

/**
 * Encapsulates commands implementing group related commands
 *
 * @author Nicky Sandhu
 * @version $Id: CreateTWReferencesCommand.java,v 1.1 2003/10/02 20:48:25
 * redwood Exp $
 */
public class CreateTWReferencesCommand implements Command
{
	private Group _group;
	private int[] _rNumbers;
	private String _timeText;
	private DataReference[] _refs;

	/**
	 * opens group and sets current group to
	 */
	public CreateTWReferencesCommand(Group g, int[] referenceNumbers,
									 String timeText)
	{
		_group = g;
		_rNumbers = referenceNumbers;
		_timeText = timeText;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException
	{
		if(_rNumbers == null || _rNumbers.length == 0)
		{
			return;
		}
		StringTokenizer tokenizer = new StringTokenizer(_timeText, "-");
		if(tokenizer.countTokens() != 2)
		{
			JOptionPane.showMessageDialog(null,
					"Incorrect time specification: " + _timeText);
			return;
		}
		TimeFactory tf = DSSUtil.getTimeFactory();
		TimeWindow tw = null;
		try
		{
			Time stTime = tf.getTimeInstance().create(
					tokenizer.nextToken().trim());
			Time endTime = tf.getTimeInstance().create(
					tokenizer.nextToken().trim());
			tw = tf.createTimeWindow(stTime, endTime);
		}
		catch(Exception e)
		{
			JOptionPane.showMessageDialog(new JFrame(), "Exception"
					+ e.getMessage() + " parsing time from: " + _timeText);
			return;
		}
		if(tw == null)
		{
			return;
		}
		_refs = new DataReference[_rNumbers.length];
		for(int i = 0; i < _rNumbers.length; i++)
		{
			DataReference ref = _group.getDataReference(_rNumbers[i]);
			tw = TimeFactory.createRoundedTimeWindow(tw, ref.getTimeInterval());
			ref = DataReference.create(ref, tw);
			_refs[i] = ref;
		}
		for(int i = _refs.length - 1; i >= 0; i--)
		{
			DataReference ref = _refs[i];
			if(ref != null)
			{
				_group.insertDataReferenceAt(_rNumbers[0], ref);
			}
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException
	{
		if(_refs == null || _refs.length == 0)
		{
			return;
		}
		for(int i = 0; i < _refs.length; i++)
		{
			if(_refs[i] != null)
			{
				_group.removeDataReference(_refs[i]);
			}
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
	}
} // end of CloneReferenceCommand

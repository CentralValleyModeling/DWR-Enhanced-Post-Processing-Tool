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

import vista.db.dss.DSSUtil;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.DataReference;
import vista.set.Group;
import vista.set.SetUtils;
import vista.set.TimeSeries;

/**
 * Encapsulates commands implementing group related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: ExportDataToDSSCommand.java,v 1.1 2003/10/02 20:48:29 redwood
 *          Exp $
 */
public class ExportDataToDSSCommand implements Command {
	private Group _group;
	private int[] _rNumbers;
	private String _filename;
	private boolean _withFlags;

	/**
	 * opens group and sets current group to
	 */
	public ExportDataToDSSCommand(Group g, int[] numbers, String filename,
			boolean withFlags) {
		_group = g;
		_rNumbers = numbers;
		_filename = filename;
		_withFlags = withFlags;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_filename == null)
			return;
		if (_rNumbers == null || _rNumbers.length == 0)
			return;
		DataReference[] refs = new DataReference[_rNumbers.length];
		for (int i = 0; i < _rNumbers.length; i++) {
			try {
				refs[i] = _group.getDataReference(_rNumbers[i]);
				DataReference ref = refs[i];

				DSSUtil.writeData(_filename, ref.getPathname().toString(),
						SetUtils.convertFlagsToValues((TimeSeries) refs[i]
								.getData()), _withFlags);
			} catch (Exception ioe) {
				throw new ExecutionException(ioe,
						"exception exporting data to dss");
			}
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		throw new ExecutionException("Cannot undo write to data base");
	}

	/**
	 * checks if command is executable.
	 */
	public boolean isUnexecutable() {
		return false;
	}

	/**
	 * writes to script
	 */
	public void toScript(StringBuffer buf) {
	}
} // end of ExportDataCommand

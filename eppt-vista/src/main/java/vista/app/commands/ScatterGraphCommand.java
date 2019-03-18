/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.commands;

import vista.app.DataGraphFrame;
import vista.app.DefaultGraphBuilder;
import vista.app.MultiScatterGraph;
import vista.graph.Graph;
import vista.gui.Command;
import vista.gui.ExecutionException;
import vista.set.DataReference;
import vista.set.Group;
import vista.set.ProxyFactory;

/**
 * Encapsulates commands implementing group related commands
 * 
 * @author Nicky Sandhu
 * @version $Id: ScatterGraphCommand.java,v 1.1 2003/10/02 20:48:40 redwood Exp
 *          $
 */
public class ScatterGraphCommand implements Command {
	private Group _group;
	private int[] _rNumbers;
	private String _filename;
	private DataReference _reference;

	/**
	 * opens group and sets current group to
	 */
	public ScatterGraphCommand(Group g, int[] referenceNumbers) {
		_group = g;
		_rNumbers = referenceNumbers;
	}

	/**
	 * executes command
	 */
	public void execute() throws ExecutionException {
		if (_rNumbers == null || _rNumbers.length == 0)
			return;
		if (_rNumbers.length == 1)
			return;
		if (_rNumbers.length == 2) {
			DefaultGraphBuilder gb = new DefaultGraphBuilder();
			for (int i = 0; i < _rNumbers.length; i++) {
				for (int j = i + 1; j < _rNumbers.length; j++) {
					DataReference ref1 = _group.getDataReference(_rNumbers[i]);
					DataReference ref2 = _group.getDataReference(_rNumbers[j]);
					DataReference ref = ProxyFactory
							.createPairedTimeSeriesProxy(ref1, ref2);
					gb.addData(ref);
				}
			}
			Graph[] graphs = gb.createGraphs();
			if (graphs != null || graphs.length > 0) {
				for (int i = 0; i < graphs.length; i++) {
					new DataGraphFrame(graphs[i], "Graph").setVisible(true);
				}
			}
		} else {
			// check to get only regular time series
			DataReference[] refs = new DataReference[_rNumbers.length];
			for (int i = 0; i < refs.length; i++) {
				refs[i] = _group.getDataReference(_rNumbers[i]);
			}
			new MultiScatterGraph(refs);
		}
	}

	/**
	 * unexecutes command or throws exception if not unexecutable
	 */
	public void unexecute() throws ExecutionException {
		throw new ExecutionException("Cannot undo graphing of data");
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
} // end of GraphDataCommand

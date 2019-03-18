/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.schematic;

import java.awt.Rectangle;

import vista.graph.Scale;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;

/**
   * 
   */
public class GridLabelElement extends TextLine {
	/**
   * 
   */
	public GridLabelElement(TextLineAttr tla, String label) {
		super(tla, label);
	}

	/**
   * 
   */
	public void setGrid(DSMGridElement grid) {
		_grid = grid;
	}

	/**
   * 
   */
	public void setBaseNode(int nodeId) {
		_node = _grid.getNetwork().getNode(nodeId);
	}

	/**
   * 
   */
	public void setBounds(Rectangle r) {
		if (_grid != null) {
			Rectangle rb = getBounds();
			Scale xS = _grid.getXScale();
			Scale yS = _grid.getYScale();

			rb.x = xS.scaleToUC(_node.getX());
			rb.y = yS.scaleToUC(_node.getY());
			super.setBounds(rb);
		} else {
			super.setBounds(r);
		}
	}

	/**
   * 
   */
	private DSMGridElement _grid;
	/**
   * 
   */
	private Node _node;
}

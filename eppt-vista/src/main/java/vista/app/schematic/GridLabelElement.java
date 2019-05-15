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
package vista.app.schematic;

import java.awt.Rectangle;

import vista.graph.Scale;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;

/**
 *
 */
public class GridLabelElement extends TextLine
{
	/**
	 *
	 */
	private DSMGridElement _grid;
	/**
	 *
	 */
	private Node _node;

	/**
	 *
	 */
	public GridLabelElement(TextLineAttr tla, String label)
	{
		super(tla, label);
	}

	/**
	 *
	 */
	public void setGrid(DSMGridElement grid)
	{
		_grid = grid;
	}

	/**
	 *
	 */
	public void setBaseNode(int nodeId)
	{
		_node = _grid.getNetwork().getNode(nodeId);
	}

	/**
	 *
	 */
	public void setBounds(Rectangle r)
	{
		if(_grid != null)
		{
			Rectangle rb = getBounds();
			Scale xS = _grid.getXScale();
			Scale yS = _grid.getYScale();

			rb.x = xS.scaleToUC(_node.getX());
			rb.y = yS.scaleToUC(_node.getY());
			super.setBounds(rb);
		}
		else
		{
			super.setBounds(r);
		}
	}
}

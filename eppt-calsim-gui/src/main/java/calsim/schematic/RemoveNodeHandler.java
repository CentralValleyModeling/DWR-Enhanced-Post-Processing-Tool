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
package calsim.schematic;

import java.awt.event.MouseEvent;

import calsim.gym.Network;
import calsim.gym.Node;

///import vista.graph.*;
//import javax.swing.*;
//import java.awt.*;
//import java.util.*;
//import calsim.gui.*;

/**
 * An interface defining the call back functions when the schematic
 * recives meaninful input
 *
 * @author Nicky Sandhu
 * @version $Id: RemoveNodeHandler.java,v 1.1.2.2 1999/07/13 16:14:33 nsandhu Exp $
 */
public class RemoveNodeHandler implements RequestHandler
{
	private CalsimSchematic _cs;
	private CalsimSchematicCanvas _csc;
	private NetworkSchematicData _nsd;

	//  private GraphicElement _nodeElement;
	//  private String _label, _type;
	public RemoveNodeHandler(CalsimSchematicCanvas csc)
	{
		_csc = csc;
		_cs = _csc.getSchematic();
		_nsd = (NetworkSchematicData) _cs.getModel();
	}

	/**
	 * called when a symbol is dragged
	 */
	public void draggedTo(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is clicked on
	 */
	public void clickedOn(Object obj, MouseEvent e)
	{
		// get node under this point else quit
		String nodeNum = null;
		if(obj instanceof String)
		{
			String nm = (String) obj;
			if(nm.indexOf("storage") >= 0)
			{
				nodeNum = nm.substring(nm.indexOf("storage") + 8).trim();
			}
			else if(nm.indexOf("junction") >= 0)
			{
				nodeNum = nm.substring(nm.indexOf("junction") + 9).trim();
			}
			else if(nm.indexOf("hidden") >= 0)
			{
				nodeNum = nm.substring(nm.indexOf("hidden") + 7).trim();
			}
			else
			{
			}
		}
		if(nodeNum == null)
		{
			return;
		}
		Network net = _nsd.getNetwork();
		Node node = net.getNode(nodeNum);
		if(node == null)
		{
			return;
		}
		// remove from network.. call this first as it throws an exception if
		// node is connected to stuff.
		net.remove(node);
		// remove from schematic
		_cs.removeSymbol(_cs.getSymbol(obj));
		// redraw schematic
		_csc.redraw();
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void doubleClickedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void pressedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void releasedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is moved over
	 */
	public void movedOver(Object obj, MouseEvent e)
	{
	}

}

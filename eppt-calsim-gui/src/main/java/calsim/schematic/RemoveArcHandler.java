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

import calsim.gym.Arc;
import calsim.gym.Network;

//import vista.graph.*;
//import javax.swing.*;
//import java.awt.*;
//import java.util.*;
//import calsim.gui.*;

/**
 * An interface defining the call back functions when the schematic
 * recives meaninful input
 *
 * @author Nicky Sandhu
 * @version $Id: RemoveArcHandler.java,v 1.1.2.2 1999/07/13 16:14:33 nsandhu Exp $
 */
public class RemoveArcHandler implements RequestHandler
{
	private CalsimSchematic _cs;
	private CalsimSchematicCanvas _csc;
	private NetworkSchematicData _nsd;

	public RemoveArcHandler(CalsimSchematicCanvas csc)
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
		// get arc under this point else quit
		String arcNum = null;
		if(obj instanceof String)
		{
			String nm = (String) obj;
			if(nm.indexOf("inflow") >= 0)
			{
				arcNum = nm.substring(nm.indexOf("inflow") + 7).trim();
			}
			else if(nm.indexOf("flow") >= 0)
			{
				arcNum = nm.substring(nm.indexOf("flow") + 5).trim();
			}
			else if(nm.indexOf("demand") >= 0)
			{
				arcNum = nm.substring(nm.indexOf("demand") + 7).trim();
			}
			else
			{
				//throw new RuntimeException("Unknown type of arc: " + nm);
			}
		}
		if(arcNum == null)
		{
			return;
		}
		// get the network and the arc
		Network net = _nsd.getNetwork();
		Arc arc = net.getArc(arcNum);
		if(arc == null)
		{
			return;
		}
		// remove from schematic
		System.out.println("Removing arc " + arcNum);
		System.out.println("Removing " + obj);
		System.out.println("Removing symbol " + _cs.getSymbol(obj));
		_cs.removeSymbol(_cs.getSymbol(obj));
		// remove from network
		net.remove(arc);
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

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

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;

import calsim.gym.Arc;
import calsim.gym.ChannelArc;
import calsim.gym.DemandArc;
import calsim.gym.InputArc;
import calsim.gym.Network;
import calsim.gym.Node;
import calsim.gym.ReturnArc;
import vista.graph.GraphicElement;

//import javax.swing.*;
//import java.util.*;
//import calsim.gui.*;

/**
 * An interface defining the call back functions when the schematic
 * recives meaninful input
 *
 * @author Nicky Sandhu
 * @version $Id: AddArcHandler.java,v 1.1.2.1 1999/06/01 16:58:42 nsandhu Exp $
 */
public class AddArcHandler implements RequestHandler
{
	private CalsimSchematic _cs;
	private CalsimSchematicCanvas _csc;
	private NetworkSchematicData _nsd;
	private GraphicElement _arcElement;
	private String _label, _type;
	private boolean _firstClickDone, _secondClickDone;

	public AddArcHandler(CalsimSchematicCanvas csc, String label, String type)
	{
		_csc = csc;
		_firstClickDone = false;
		_secondClickDone = false;
		_cs = _csc.getSchematic();
		_nsd = (NetworkSchematicData) _cs.getModel();
		_arcElement = null;
		_label = label.trim().toUpperCase();
		_type = type.trim();
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
		if(!_firstClickDone)
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
					throw new RuntimeException("Object is not a node " + nm);
				}
			}
			if(nodeNum == null)
			{
				return;
			}
			Network net = _nsd.getNetwork();
			Node bn = net.getNode(nodeNum);
			if(bn == null)
			{
				throw new RuntimeException(bn + " no such node in schematic?");
			}
			// add the arc to the network
			Arc arc = null;
			if(_type.equals("flow"))
			{
				arc = new ChannelArc(_label, null, null);
			}
			else if(_type.equals("inflow"))
			{
				arc = new InputArc(_label, null, null);
			}
			else if(_type.equals("demand"))
			{
				arc = new DemandArc(_label, null, null);
			}
			else if(_type.equals("return"))
			{
				arc = new ReturnArc(_label, null, null);
			}
			else
			{
				throw new RuntimeException("Unknown type of arc : " + _type + " with label: " + _label);
			}
			net.add(arc);
			arc.setUpstreamNode(bn);
			bn.addArc(arc);
			// create the element
			if(_arcElement == null)
			{
				_arcElement = _nsd.createArc(_label, _type);
			}
			Rectangle r = _arcElement.getBounds();
			r.x = e.getX();
			r.y = e.getY();
			_firstClickDone = true;
		}
		else if(!_secondClickDone)
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
					throw new RuntimeException("Object is not a node " + nm);
				}
			}
			if(nodeNum == null)
			{
				return;
			}
			Network net = _nsd.getNetwork();
			Node tn = net.getNode(nodeNum);
			if(tn == null)
			{
				throw new RuntimeException(tn + " no such node in schematic?");
			}
			Arc arc = net.getArc(_label);
			tn.addArc(arc);
			arc.setDownstreamNode(tn);
			//
			CalsimSymbolData[] csdArray = _nsd.updateArc(_label);
			_cs.addedToModel(csdArray);
			// no need to draw the whole schematic
			movedOver(obj, e);
			_secondClickDone = true;
			_csc.redraw();
		}
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
		if(_firstClickDone && !_secondClickDone)
		{
			// position it
			Rectangle r = _arcElement.getBounds();
			r.width = e.getX() - r.x;
			r.height = e.getY() - r.y;
			// get the schematic image
			Graphics cg = _csc.getCanvas().getGraphics();
			Image img = _csc.getCanvas().getGraphicElementImage();
			cg.drawImage(img, 0, 0, _csc.getCanvas());
			// draw it
			_arcElement.draw(cg, r);
		}
	}

}

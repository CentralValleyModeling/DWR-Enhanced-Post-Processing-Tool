/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.schematic;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;

import calsim.gym.Network;
import vista.graph.GraphicElement;

//import javax.swing.*;
//import java.util.*;
//import calsim.gui.*;

/**
 * An interface defining the call back functions when the schematic
 * recives meaninful input
 *
 * @author Nicky Sandhu
 * @version $Id: AddNodeHandler.java,v 1.1.2.1 1999/06/01 16:58:43 nsandhu Exp $
 */
public class AddNodeHandler implements RequestHandler
{
	private CalsimSchematic _cs;
	private CalsimSchematicCanvas _csc;
	private NetworkSchematicData _nsd;
	private GraphicElement _nodeElement;
	private String _label, _type;
	private boolean _clickedOnce;

	public AddNodeHandler(CalsimSchematicCanvas csc, String label, String type)
	{
		_csc = csc;
		_cs = _csc.getSchematic();
		_nsd = (NetworkSchematicData) _cs.getModel();
		_nodeElement = null;
		_label = label.trim().toUpperCase();
		_type = type.trim();
		if(_type.equals("hidden") && !_label.startsWith("X"))
		{
			_label = "X" + _label;
		}
		_clickedOnce = false;
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
		// draw the node element there...
		movedOver(obj, e);
		// add the node to the network and to the schematic
		Network net = _nsd.getNetwork();
		net.addNode(_label);
		if(_type.equals("storage"))
		{
			net.getNode(_label).setHasStorage(true);
		}
		float x = (float) _cs.getXScale().scaleToDC(e.getX());
		float y = (float) _cs.getYScale().scaleToDC(e.getY());
		CalsimSymbolData[] csdArray = _nsd.updateNode(_label, x, y);
		_cs.addedToModel(csdArray);
		_clickedOnce = true;
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
		if(_clickedOnce)
		{
			return;
		}
		// draw the node's graphic element at the mouse position
		// create a node element if not yet created
		if(_nodeElement == null)
		{
			_nodeElement = _nsd.createNode(_label, _type);
		}
		Rectangle r = _nodeElement.getBounds();
		r.x = e.getX();
		r.y = e.getY();
		_nodeElement.setBounds(r);
		// get the schematic image
		Graphics cg = _csc.getCanvas().getGraphics();
		Image img = _csc.getCanvas().getGraphicElementImage();
		cg.drawImage(img, 0, 0, _csc.getCanvas());
		// draw the node element on the image ( slight flickering ??)
		_nodeElement.draw(cg, r);
	}

}

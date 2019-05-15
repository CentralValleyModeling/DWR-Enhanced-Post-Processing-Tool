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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.util.StringTokenizer;

import calsim.gym.Arc;
import calsim.gym.ChannelArc;
import calsim.gym.DemandArc;
import calsim.gym.GymUtils;
import calsim.gym.InputArc;
import calsim.gym.Network;
import calsim.gym.Node;
import calsim.gym.ReturnArc;
import vista.graph.GraphicElement;
import vista.graph.Scale;

//import javax.swing.*;
//import calsim.gui.*;

/**
 * An interface defining the call back functions when the schematic
 * recives meaninful input
 *
 * @author Nicky Sandhu
 * @version $Id: DragHandler.java,v 1.1.2.5 1999/07/18 20:57:55 nsandhu Exp $
 */
public class DragHandler implements RequestHandler
{
	//  private GEScaledLayout _layout;
	private static final boolean DEBUG = false;
	private CalsimSchematic _cs;
	private CalsimSchematicCanvas _csc;
	private GraphicElement _dragee;
	private NetworkSchematicData _nsdm;
	private GraphicElement[] _geup, _gedown;
	private int _dx = 5, _dy = 5;

	public DragHandler(CalsimSchematicCanvas csc)
	{
		setSchematicCanvas(csc);
	}

	/**
	 *
	 */
	public void setSchematicCanvas(CalsimSchematicCanvas csc)
	{
		_csc = csc;
		_cs = _csc.getSchematic();
	}

	/**
	 *
	 */
	public int getDragXIncrement()
	{
		return _dx;
	}

	/**
	 *
	 */
	public void setDragXIncrement(int dx)
	{
		_dx = dx;
	}

	/**
	 *
	 */
	public int getDragYIncrement()
	{
		return _dy;
	}

	/**
	 *
	 */
	public void setDragYIncrement(int dy)
	{
		_dy = dy;
	}

	/**
	 * called when a symbol is dragged
	 */
	public void draggedTo(Object obj, MouseEvent e)
	{
		// if nothing is being dragged return.
		if(_dragee == null)
		{
			return;
		}
		// snap to grid
		Rectangle r = _dragee.getBounds();
		r.x = (e.getX() / _dx) * _dx;
		r.y = (e.getY() / _dy) * _dy;
		// if r.x or r.y is beyond the current view scroll to the show where its
		// being dragged or increase the size of the schematic itself. However
		// don't change the scale so modify the scale of the schematic as well.
		Dimension psize = _cs.getPreferredSize();
		if(r.x > psize.width || r.y > psize.height)
		{
			_cs.setPreferredSize(Math.max(r.x, psize.width), Math.max(r.y, psize.height));
			//      Scale xs = _cs.getXScale();
			//      Scale ys = _cs.getYScale();
		}
		// Get the graphics
		Graphics cg = _csc.getCanvas().getGraphics();
		// get the current clip size
		Rectangle clipRect = _csc.getScrollPane().getViewport().getViewRect();
		cg.setClip(clipRect);
		// draw the base image
		Image img = _csc.getCanvas().getGraphicElementImage();
		cg.drawImage(img, 0, 0, _csc.getCanvas());
		// draw symbol and all the other symbols that are affected
		if(_geup != null)
		{
			for(int i = 0; i < _geup.length; i++)
			{
				if(_geup[i] == null)
				{
					continue;
				}
				Rectangle ru = _geup[i].getBounds();
				ru.width = r.x - ru.x;
				ru.height = r.y - ru.y;
				_geup[i].draw(cg, ru);
			}
		}
		if(_gedown != null)
		{
			for(int i = 0; i < _gedown.length; i++)
			{
				if(_gedown[i] == null)
				{
					continue;
				}
				Rectangle rd = _gedown[i].getBounds();
				rd.width = rd.x - r.x + rd.width;
				rd.height = rd.y - r.y + rd.height;
				rd.x = r.x;
				rd.y = r.y;
				_gedown[i].draw(cg, rd);
			}
		}
		_dragee.draw(cg, r);
	}

	/**
	 * returns the graphic elements for the arcs in the arc array.
	 */
	private GraphicElement[] getElements(Arc[] arcArray)
	{
		if(arcArray == null || arcArray.length == 0)
		{
			return null;
		}
		GraphicElement[] geArray = new GraphicElement[arcArray.length];
		for(int i = 0; i < arcArray.length; i++)
		{
			geArray[i] = getElement(arcArray[i]);
		}
		return geArray;
	}

	/**
	 *
	 */
	public GraphicElement getElement(Arc arc)
	{
		String symType = "flow";
		String label = arc.getName();
		if(arc instanceof InputArc)
		{
			symType = "inflow";
		}
		else if(arc instanceof ChannelArc)
		{
			symType = "flow";
		}
		else if(arc instanceof ReturnArc)
		{
			symType = "inflow";
		}
		else if(arc instanceof DemandArc)
		{
			symType = "demand";
		}
		else
		{
		}
		String name = symType + " " + label;
		if(DEBUG)
		{
			System.out.println("Arc name = " + name);
		}
		return _cs.getSymbol(name);
	}

	/**
	 * called when a symbol with reference object obj is clicked on
	 */
	public void clickedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void doubleClickedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 *
	 */
	public void drawGrid(Graphics g, Rectangle r)
	{
		Color oldColor = g.getColor();
		g.setColor(Color.lightGray);
		int x = 0;
		int y = 0;
		while(x < r.width)
		{
			g.drawLine(x, 0, x, r.height);
			x += _dx;
		}
		while(y < r.height)
		{
			g.drawLine(0, y, r.width, y);
			y += _dy;
		}
		g.setColor(oldColor);
	}

	/**
	 * This catches hold of the symbol which is to be dragged.
	 */
	public void pressedOn(Object obj, MouseEvent e)
	{
		// if nothing under press just return
		if(obj == null)
		{
			return;
		}
		// get the symbol to be dragged...
		GraphicElement ge = _cs.getSymbol(obj);
		// if the graphic element is not a labeled symbol
		// we are not interested in dragging it...
		if(!(ge instanceof LabeledSymbol))
		{
			return;
		}
		// keep the currently dragged element
		_dragee = ge;
		// get the graphics handle for the schematic canvas
		Graphics g = _csc.getGraphics();
		// draw snap-to-grid on canvas
		drawGrid(g, _csc.getScrollPane().getViewport().getViewRect());
		// get connecting arcs and update their position as well
		// all this should be done via telling the model that
		// a node's position has been updated and letting it
		// communicate with all its views
		// model.setIsDragging(Object obj);
		_nsdm = (NetworkSchematicData) _cs.getModel();
		Network net = _nsdm.getNetwork();
		// gets the node for this object
		String nodeName = null;
		StringTokenizer st = new StringTokenizer((String) obj);
		st.nextToken();
		nodeName = st.nextToken();
		Node n1 = net.getNode(nodeName);
		if(n1 == null)
		{
			return;
		}
		// get upstream and downstream arcs
		Arc[] ua1 = GymUtils.getJustUpstreamArcs(n1);
		Arc[] da1 = GymUtils.getJustDownstreamArcs(n1);
		// get symbols for those arcs
		_geup = getElements(ua1);
		_gedown = getElements(da1);
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void releasedOn(Object obj, MouseEvent e)
	{
		// get the element being dragged
		GraphicElement ge = _dragee;
		// if clicked on a null object just return
		if(_dragee == null)
		{
			return;
		}
		//
		_cs.removeSymbol(_dragee);
		obj = _cs._somap.get(_dragee);
		_cs.addSymbol(ge, obj);
		//
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
				throw new RuntimeException("Unrecognized type of object " + nm);
			}
		}
		if(nodeNum == null)
		{
			return;
		}
		Scale xs = _cs.getXScale();
		Scale ys = _cs.getYScale();
		int xx = (e.getX() / _dx) * _dx;
		int yy = (e.getY() / _dy) * _dy;
		CalsimSymbolData[] csdArray = _nsdm.setNodeXY(nodeNum,
				(float) xs.scaleToDC(xx),
				(float) ys.scaleToDC(yy));
		if(DEBUG)
		{
			System.out.println("Node released = " + nodeNum);
		}
		_cs.modelChanged(csdArray);
		//
		_csc.redraw();
		_dragee = null;
	}

	/**
	 * called when a symbol with reference object obj is moved over
	 */
	public void movedOver(Object obj, MouseEvent e)
	{
		if(DEBUG)
		{
		}
	}

}

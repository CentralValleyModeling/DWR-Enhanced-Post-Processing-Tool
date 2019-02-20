/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.schematic;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.Enumeration;
import java.util.Vector;
import javax.swing.*;

import vista.graph.GECanvas;
import vista.gui.VistaUtils;

//import vista.set.*;
//import java.awt.image.*;

/**
 * A canvas for drawing the schematic. This canvas also has methods to
 * install handlers for mouse requests on schematic components.
 * When a mouse is clicked or moved over a particular schematic component
 * the registered RequestHandler objects appropriate methods are called.
 *
 * @author Nicky Sandhu
 * @version $Id: CalsimSchematicCanvas.java,v 1.1.2.6 1999/07/18 20:57:54 nsandhu Exp $
 * @see RequestHandler
 */
public class CalsimSchematicCanvas extends JPanel
{
	//  private static boolean DEBUG = false;
	public GECanvas _canvas;
	public JScrollPane _scrollPane;
	//  private Object _currentObject = null;
	public int xi, yi, wi, hi;
	private CalsimSchematic _sc;
	private Vector _handlers;

	/**
	 *
	 */
	public CalsimSchematicCanvas(CalsimSchematic sc)
	{
		//
		_handlers = new Vector();
		// add the schematic canvas
		_sc = sc;
		//
		_canvas = new GECanvas(_sc);
		_canvas.setDoubleBuffered(true);
		_canvas.setClipped(false);
		setLayout(new BorderLayout());
		_scrollPane = new JScrollPane(_canvas);
		add(_scrollPane, BorderLayout.CENTER);
		// add mouse listeners to this component
		_canvas.addMouseListener(new MouseAdapter()
		{
			public void mouseClicked(MouseEvent e)
			{
				int x = e.getX();
				int y = e.getY();
				int cc = e.getClickCount();
				Object obj = _sc.getHitElementObject(x, y);
				if(cc == 1)
				{
					clickedOn(obj, e);
				}
				else
				{
					doubleClickedOn(obj, e);
				}
			}

			public void mousePressed(MouseEvent e)
			{
				int x = e.getX();
				int y = e.getY();
				Object obj = _sc.getHitElementObject(x, y);
				pressedOn(obj, e);
			}

			public void mouseReleased(MouseEvent e)
			{
				int x = e.getX();
				int y = e.getY();
				Object obj = _sc.getHitElementObject(x, y);
				releasedOn(obj, e);
			}
		});
		//
		_canvas.addMouseMotionListener(new MouseMotionAdapter()
		{
			public void mouseMoved(MouseEvent e)
			{
				int x = e.getX();
				int y = e.getY();
				Object obj = _sc.getHitElementObject(x, y);
				movedOver(obj, e);
			}

			public void mouseDragged(MouseEvent e)
			{
				//	int x = e.getX(); int y = e.getY();
				//	Object obj = _sc.getHitElementObject(x,y);
				draggedTo(null, e);
			}
		});
	}

	/**
	 *
	 */
	public void redraw()
	{
		Frame fr = JOptionPane.getFrameForComponent(getCanvas());
		Cursor oldCursor = fr.getCursor();
		fr.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		getCanvas().redoNextPaint();
		fr.paint(fr.getGraphics());
		fr.setCursor(oldCursor);
	}

	/**
	 *
	 */
	public JScrollPane getScrollPane()
	{
		return _scrollPane;
	}

	/**
	 *
	 */
	public CalsimSchematic getSchematic()
	{
		return _sc;
	}

	/**
	 *
	 */
	public GECanvas getCanvas()
	{
		return _canvas;
	}

	/**
	 *
	 */
	public void add(RequestHandler rh)
	{
		_handlers.addElement(rh);
	}

	/**
	 *
	 */
	public void remove(RequestHandler rh)
	{
		_handlers.removeElement(rh);
	}

	/**
	 *
	 */
	public void removeAllHandlers()
	{
		_handlers.removeAllElements();
	}

	/**
	 *
	 */
	public void clickedOn(Object obj, MouseEvent evt)
	{
		for(Enumeration e = _handlers.elements(); e.hasMoreElements(); )
		{
			RequestHandler rh = (RequestHandler) e.nextElement();
			try
			{
				rh.clickedOn(obj, evt);
			}
			catch(Exception exc)
			{
				VistaUtils.displayException(this, exc);
			}
		}
	}

	/**
	 *
	 */
	public void doubleClickedOn(Object obj, MouseEvent evt)
	{
		for(Enumeration e = _handlers.elements(); e.hasMoreElements(); )
		{
			RequestHandler rh = (RequestHandler) e.nextElement();
			try
			{
				rh.doubleClickedOn(obj, evt);
			}
			catch(Exception exc)
			{
				VistaUtils.displayException(this, exc);
			}
		}
	}

	/**
	 *
	 */
	public void movedOver(Object obj, MouseEvent evt)
	{
		for(Enumeration e = _handlers.elements(); e.hasMoreElements(); )
		{
			RequestHandler rh = (RequestHandler) e.nextElement();
			try
			{
				rh.movedOver(obj, evt);
			}
			catch(Exception exc)
			{
				VistaUtils.displayException(this, exc);
			}
		}
	}

	/**
	 *
	 */
	public void pressedOn(Object obj, MouseEvent evt)
	{
		for(Enumeration e = _handlers.elements(); e.hasMoreElements(); )
		{
			RequestHandler rh = (RequestHandler) e.nextElement();
			try
			{
				rh.pressedOn(obj, evt);
			}
			catch(Exception exc)
			{
				VistaUtils.displayException(this, exc);
			}
		}
	}

	/**
	 *
	 */
	public void releasedOn(Object obj, MouseEvent evt)
	{
		for(Enumeration e = _handlers.elements(); e.hasMoreElements(); )
		{
			RequestHandler rh = (RequestHandler) e.nextElement();
			try
			{
				rh.releasedOn(obj, evt);
			}
			catch(Exception exc)
			{
				VistaUtils.displayException(this, exc);
			}
		}
	}

	/**
	 *
	 */
	public void draggedTo(Object obj, MouseEvent evt)
	{
		for(Enumeration e = _handlers.elements(); e.hasMoreElements(); )
		{
			RequestHandler rh = (RequestHandler) e.nextElement();
			try
			{
				rh.draggedTo(null, evt);
			}
			catch(Exception exc)
			{
				VistaUtils.displayException(this, exc);
			}
		}
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Rectangle;
import java.util.Stack;

//import COM.objectspace.jgl.Stack;

/**
 * This class handles the zooming in and zooming out on Graph class. It should
 * also store state as it zooms in so as to restore it on zoom outs.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: Zoom.java,v 1.1 2003/10/02 20:49:12 redwood Exp $
 */
public class Zoom
{
	public static final int TOP = AxisAttr.TOP;
	public static final int BOTTOM = AxisAttr.BOTTOM;
	public static final int RIGHT = AxisAttr.RIGHT;
	public static final int LEFT = AxisAttr.LEFT;
	private static final boolean DEBUG = false;
	private Plot _plot;
	/**
	 * defines the percentage of overlap between current and next page
	 */
	private double pagingSensitivity = 0.5;
	/**
	 *
	 */
	private ZoomState _pagingZoomState = null;
	/**
	 * Current zooming region
	 */
	private Rectangle _currentZoom = null, _initialZoom = null;
	/**
	 * Records zoom history
	 */
	private Stack _zoomHistory = new Stack();

	/**
	 * Constructor
	 */
	public Zoom(Plot plot)
	{
		_plot = plot;
	}

	/**
	 * gets the plot on which this zoom is acting upon...
	 */
	public Plot getPlot()
	{
		return _plot;
	}

	/**
	 * Returns the currently selected zoom rectangle
	 */
	public Rectangle getZoomRectangle()
	{
		return _plot.getCurveContainer().getBounds();
	}

	/**
	 * a change in page based on the change in the x and y co-ordinates of the
	 * four corners of the current zoom.
	 */
	public void nextPage(int delx, int dely)
	{
		if(DEBUG)
		{
			System.out.println("Next Page:");
		}
		if(_currentZoom == null)
		{
			return;
		}
		zoomPage(delx, dely);
		if(DEBUG)
		{
			System.out.println("Zoom Rect: " + _currentZoom);
		}
	}

	/**
	 *
	 */
	public void zoomPage(int delx, int dely)
	{
		if(_zoomHistory.isEmpty())
		{
			return; // return if not in zooming mode
		}
		if(_pagingZoomState == null)
		{
			_pagingZoomState = new ZoomState(_plot, _currentZoom);
		}
		ZoomState czs = null;
		if(_zoomHistory.size() == 1)
		{
			czs = null;
		}
		else
		{
			czs = (ZoomState) _zoomHistory.peek();
		}
		boolean useMinMax = true;
		if(DEBUG)
		{
			System.out.println("delx=" + delx + " dely=" + dely);
		}
		Axis axis = _plot.getAxis(AxisAttr.TOP);
		if(axis != null)
		{
			Scale scale = _pagingZoomState.getScale(AxisAttr.TOP);
			double dt = scale.getDataMinimum();
			scale.translate(delx);
			dt = scale.getDataMinimum() - dt;
			if(czs != null)
			{
				czs.getScale(AxisAttr.TOP).translate(dt);
			}
			setAxisRange(axis, scale.getDataMinimum(), scale.getDataMaximum(),
					useMinMax);
		}
		axis = _plot.getAxis(AxisAttr.BOTTOM);
		if(axis != null)
		{
			Scale scale = _pagingZoomState.getScale(AxisAttr.BOTTOM);
			double dt = scale.getDataMinimum();
			scale.translate(delx);
			dt = scale.getDataMinimum() - dt;
			if(czs != null)
			{
				czs.getScale(AxisAttr.BOTTOM).translate(dt);
			}
			if(DEBUG)
			{
				System.out.println(scale);
			}
			setAxisRange(axis, scale.getDataMinimum(), scale.getDataMaximum(),
					useMinMax);
			if(DEBUG)
			{
				System.out.println(scale);
			}
		}
		axis = _plot.getAxis(AxisAttr.LEFT);
		if(axis != null)
		{
			Scale scale = _pagingZoomState.getScale(AxisAttr.LEFT);
			double dt = scale.getDataMinimum();
			scale.translate(dely);
			dt = scale.getDataMinimum() - dt;
			if(czs != null)
			{
				czs.getScale(AxisAttr.LEFT).translate(dt);
			}
			setAxisRange(axis, scale.getDataMinimum(), scale.getDataMaximum(),
					useMinMax);
		}

		axis = _plot.getAxis(AxisAttr.RIGHT);
		if(axis != null)
		{
			Scale scale = _pagingZoomState.getScale(AxisAttr.RIGHT);
			double dt = scale.getDataMinimum();
			scale.translate(dely);
			dt = scale.getDataMinimum() - dt;
			if(czs != null)
			{
				czs.getScale(AxisAttr.RIGHT).translate(dt);
			}
			setAxisRange(axis, scale.getDataMinimum(), scale.getDataMaximum(),
					useMinMax);
		}

	}

	/**
	 * @param overlap in percentage from 0.0 to 1.0 overlap = 0.0 means full overlap
	 *                or zero paging motion overlap = 1.0 means no overlap.
	 */
	public void setPagingOverlap(double overlap)
	{
		if(overlap <= 0.0 || overlap > 1.0)
		{
			return;
		}
		pagingSensitivity = overlap;
	}

	/**
	 * zooms forward by the rectangle size to the right of the current zoom
	 * rectangle.
	 *
	 * @param zoomRect     The rectangle coordinates in screen system to be zoomed into
	 * @param nextPosition TOP, BOTTOM, LEFT, RIGHT
	 */
	public void nextPage(int nextPosition)
	{
		if(_currentZoom == null)
		{
			return;
		}
		Rectangle r = _plot.getCurveContainer().getBounds();
		int x = 0, y = 0;
		switch(nextPosition)
		{
			case TOP:
				y = (int) (-r.height * pagingSensitivity);
				break;
			case BOTTOM:
				y = (int) (r.height * pagingSensitivity);
				break;
			case LEFT:
				x = (int) (-r.width * pagingSensitivity);
				break;
			case RIGHT:
				x = (int) (r.width * pagingSensitivity);
				break;
			default:
				x = (int) (r.width * pagingSensitivity);
		}
		//
		zoomPage(x, y);
	}

	/**
	 * resets the zoom to the original rectangle
	 */
	public void zoomReset()
	{
		zoomTo(_initialZoom);
		_currentZoom = _initialZoom;
	}

	/**
	 * Zooms in using current zoom rectangle region.
	 */
	public void zoomIn(Rectangle zoomRect)
	{
		_initialZoom = zoomRect;
		_currentZoom = _initialZoom;
		_zoomHistory.push(new ZoomState(_plot, _currentZoom));
		zoomTo(_currentZoom);
		_pagingZoomState = null;
		if(DEBUG)
		{
			System.out.println("Zoom Rect: " + _currentZoom);
		}
	}

	/**
	 * zoom to rectangle in question using previous rectangle zooms scales.
	 */
	private void zoomTo(Rectangle zr)
	{
		if(_zoomHistory.isEmpty())
		{
			return;
		}
		boolean useMinMax = true;
		if(_zoomHistory.isEmpty())
		{
			useMinMax = false;
		}
		ZoomState zs = (ZoomState) _zoomHistory.peek();
		if(DEBUG)
		{
			System.out.println("Zoom To| zoom state: " + zs);
		}
		Axis axis = _plot.getAxis(AxisAttr.TOP);
		if(axis != null)
		{
			Scale scale = zs.getScale(axis.getPosition());
			setAxisRange(axis, scale.scaleToDC(zr.x), scale.scaleToDC(zr.x
					+ zr.width), useMinMax);
		}
		axis = _plot.getAxis(AxisAttr.BOTTOM);
		if(axis != null)
		{
			Scale scale = zs.getScale(axis.getPosition());
			setAxisRange(axis, scale.scaleToDC(zr.x), scale.scaleToDC(zr.x
					+ zr.width), useMinMax);
		}

		axis = _plot.getAxis(AxisAttr.LEFT);
		if(axis != null)
		{
			Scale scale = zs.getScale(axis.getPosition());
			setAxisRange(axis, scale.scaleToDC(zr.y + zr.height), scale
					.scaleToDC(zr.y), useMinMax);
		}

		axis = _plot.getAxis(AxisAttr.RIGHT);
		if(axis != null)
		{
			Scale scale = zs.getScale(axis.getPosition());
			setAxisRange(axis, scale.scaleToDC(zr.y + zr.height), scale
					.scaleToDC(zr.y), useMinMax);
		}
	}

	public boolean isZoomedOutAllTheWay()
	{
		return _zoomHistory.isEmpty();
	}

	/**
	 * zoom out to previous state
	 */
	public boolean zoomOut()
	{
		if(_zoomHistory.isEmpty())
		{
			return false;
		}
		//
		ZoomState zs = (ZoomState) _zoomHistory.pop();
		boolean useMinMax = true;
		if(DEBUG)
		{
			System.out.println("Zoom Out| zoom state: " + zs);
		}
		//
		if(_zoomHistory.isEmpty())
		{
			useMinMax = false;
		}
		//
		if(_zoomHistory.isEmpty())
		{
			_currentZoom = null;
		}
		else
		{
			_currentZoom = ((ZoomState) _zoomHistory.peek())._zoomRect;
		}
		Plot plot = _plot;
		Axis axis = plot.getAxis(AxisAttr.TOP);
		if(axis != null)
		{
			Scale scale = zs.getScale(axis.getPosition());
			setAxisRange(axis, scale.getDataMinimum(), scale.getDataMaximum(),
					useMinMax);
		}

		axis = plot.getAxis(AxisAttr.BOTTOM);
		if(axis != null)
		{
			Scale scale = zs.getScale(axis.getPosition());
			setAxisRange(axis, scale.getDataMinimum(), scale.getDataMaximum(),
					useMinMax);
		}

		axis = plot.getAxis(AxisAttr.LEFT);
		if(axis != null)
		{
			Scale scale = zs.getScale(axis.getPosition());
			setAxisRange(axis, scale.getDataMinimum(), scale.getDataMaximum(),
					useMinMax);
		}

		axis = plot.getAxis(AxisAttr.RIGHT);
		if(axis != null)
		{
			Scale scale = zs.getScale(axis.getPosition());
			setAxisRange(axis, scale.getDataMinimum(), scale.getDataMaximum(),
					useMinMax);
		}
		_pagingZoomState = null;
		return true;
	}

	/**
	 * sets axis range with given minimum and maximum values
	 */
	private void setAxisRange(Axis axis, double minX, double maxX,
							  boolean useMinMax)
	{
		if(axis != null)
		{
			axis.getTickGenerator().useDataMinMax(useMinMax);
			axis.setDCRange(minX, maxX);
		}
	}

	/**
	 *
	 */
	private class ZoomState
	{
		/**
		 * Scale objects for all four axes
		 */
		public Scale[] _scales = new Scale[4];
		Rectangle _zoomRect;

		/**
		 * stores the zoom rectangle and the scale associated with it.
		 */
		public ZoomState(Plot plot, Rectangle zoomRect)
		{
			_zoomRect = zoomRect;
			for(int i = 0; i < plot.getElementCount(); i++)
			{
				GraphicElement ge = plot.getElement(i);
				if(ge instanceof Axis)
				{
					Axis axis = (Axis) ge;
					_scales[axis.getPosition() - AxisAttr.BOTTOM] = axis
							.getScale();
				}
			}
		}

		/**
		 *
		 */
		public Scale getScale(int pos)
		{
			return _scales[pos - AxisAttr.BOTTOM];
		}

		/**
		 *
		 */
		public String toString()
		{
			String ls = System.getProperty("line.separator");
			String str = "Zoom Rect: " + _zoomRect + ls;
			str += "Scales: " + ls;
			for(int i = 0; i < _scales.length; i++)
			{
				str += _scales[i] + ls;
			}
			return str;
		}
	}
}

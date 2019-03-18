/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Polygon;

/**
 * Attributes for a symbol
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: SymbolAttr.java,v 1.1 2003/10/02 20:49:09 redwood Exp $
 */
public class SymbolAttr extends LineElementAttr {
	/**
	 * indicates if symbol polygon is filled
	 */
	public boolean _isFilled = false;

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * SymbolAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof SymbolAttr) {
			SymbolAttr lea = (SymbolAttr) ga;
			lea._isFilled = this._isFilled;
		}
	}

	/**
	 * sets IsFilled
	 */
	public void setIsFilled(boolean isFilled) {
		_isFilled = isFilled;
	}

	/**
	 * gets IsFilled
	 */
	public boolean getIsFilled() {
		return _isFilled;
	}

	/**
	 * The symbol is defined by a polygon. The polygon is defined by a series of
	 * points (x,y). These points are connected in succession to form the
	 * outline of the symbol. These x and y points are such that 0 <= x,y < 25.
	 * In other words an area of 25 X 25 is used for the initial definition.
	 * This would be the size of the bounding box for this polygon.
	 * 
	 * @param xc
	 *            Array of x co-ordinates 0 <= x < 25
	 * @param yc
	 *            Array of y co-ordinates 0 <= y < 25
	 */
	public void setSymbol(int[] xc, int[] yc, int np) {
		_symbolPolygon.xpoints = xc;
		_symbolPolygon.ypoints = yc;
		_symbolPolygon.npoints = np;
	}

	/**
   *
   */
	public void setSymbol(Polygon p) {
		_symbolPolygon = p;
	}

	/**
	 * gets the symbol shape.
	 * 
	 * @return a Polygon object representing the shape
	 */
	public Polygon getSymbol() {
		return _symbolPolygon;
	}

	/**
	 * Polygon for symbol shape.
	 */
	private Polygon _symbolPolygon = new Polygon();
}

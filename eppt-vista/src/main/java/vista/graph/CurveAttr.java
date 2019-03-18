/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;


/**
 * Attributes for the Curve Element
 * 
 * @see Curve
 * @author Nicky Sandhu (DWR).
 * @version $Id: CurveAttr.java,v 1.1 2003/10/02 20:48:51 redwood Exp $
 */
public class CurveAttr extends GEAttr {
	/**
	 * thickness of line in pixels
	 */
	public float _thickness = 1;
	/**
	 * style of line such as plain, dashed, dotted, etcetra.
	 */
	public float[] _dashArray = new float[] { 1 };
	/**
	 * Name of this curve.
	 */
	public String _curveName;
	/**
	 * indicates if symbol is to be drawn for given data points
	 */
	public boolean _drawSymbol = false;
	/**
	 * indicates if data points should be connected.
	 */
	public boolean _drawLines = true;
	/**
	 * number of data points to be skipped before drawing a symbol
	 */
	public int _dataPerSymbol = 5;
	/**
	 * attribute of the symbols for this curve
	 */
	public Symbol _symbol = new Symbol(new SymbolAttr());

	/**
	 * sets DrawSymbol
	 */
	public void setDrawSymbol(boolean drawSymbol) {
		_drawSymbol = drawSymbol;
	}

	/**
	 * gets DrawSymbol
	 */
	public boolean getDrawSymbol() {
		return _drawSymbol;
	}

	/**
	 * sets DrawLines
	 */
	public void setDrawLines(boolean drawLines) {
		_drawLines = drawLines;
	}

	/**
	 * gets DrawLines
	 */
	public boolean getDrawLines() {
		return _drawLines;
	}

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * CurveAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof CurveAttr) {
			CurveAttr ca = (CurveAttr) ga;
			ca._drawSymbol = this._drawSymbol;
			ca._drawLines = this._drawLines;
		}
	}

}

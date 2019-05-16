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
package vista.graph;


/**
 * Attributes for the Curve Element
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: CurveAttr.java,v 1.1 2003/10/02 20:48:51 redwood Exp $
 * @see Curve
 */
public class CurveAttr extends GEAttr
{
	/**
	 * thickness of line in pixels
	 */
	public float _thickness = 1;
	/**
	 * style of line such as plain, dashed, dotted, etcetra.
	 */
	public float[] _dashArray = new float[]{1};
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
	 * gets DrawSymbol
	 */
	public boolean getDrawSymbol()
	{
		return _drawSymbol;
	}

	/**
	 * sets DrawSymbol
	 */
	public void setDrawSymbol(boolean drawSymbol)
	{
		_drawSymbol = drawSymbol;
	}

	/**
	 * gets DrawLines
	 */
	public boolean getDrawLines()
	{
		return _drawLines;
	}

	/**
	 * sets DrawLines
	 */
	public void setDrawLines(boolean drawLines)
	{
		_drawLines = drawLines;
	}

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * CurveAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga)
	{
		super.copyInto(ga);
		if(ga instanceof CurveAttr)
		{
			CurveAttr ca = (CurveAttr) ga;
			ca._drawSymbol = this._drawSymbol;
			ca._drawLines = this._drawLines;
		}
	}

}

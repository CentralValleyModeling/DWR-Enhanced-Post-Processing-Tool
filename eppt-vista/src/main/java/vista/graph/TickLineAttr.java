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

import java.awt.Color;

/**
 * Attributes of TickLine
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: TickLineAttr.java,v 1.1 2003/10/02 20:49:11 redwood Exp $
 * @see TickLine
 */
public class TickLineAttr extends GEAttr
{
	/**
	 * Tick Line position in plot towards bottom
	 */
	public final static int BOTTOM = 1;
	/**
	 * Tick Line position in plot towards top
	 */
	public final static int TOP = BOTTOM + 1;
	/**
	 * Tick Line position in plot towards left
	 */
	public final static int LEFT = TOP + 1;
	/**
	 * Tick Line position in plot towards right
	 */
	public final static int RIGHT = LEFT + 1;
	/**
	 * Tick marks within the drawing area
	 */
	public final static int INSIDE = 10;
	/**
	 * Tick marks outside the drawing area
	 */
	public final static int OUTSIDE = INSIDE + 1;
	/**
	 * Tick marks inside & outside the drawing area
	 */
	public final static int BOTH = OUTSIDE + 1;
	/**
	 * Position of tick line in plot
	 */
	public int _position = BOTTOM;
	/**
	 * Location of tick marks within or outside bounds
	 */
	public int _tickLocation = OUTSIDE;
	/**
	 * length of major tick mark in percentage of the drawing area.
	 */
	public double _percentMajorTickLength = 0.8;
	/**
	 * length of major 2 tick mark in percentage of the drawing area.
	 */
	public double _percentMajor2TickLength = 0.0;
	/**
	 * length of minor tick mark in percentage of the drawing area.
	 */
	public double _percentMinorTickLength = 0.3;
	/**
	 * plot major ticks
	 */
	public boolean _plotMajorTicks = true;
	/**
	 * plot minor ticks
	 */
	public boolean _plotMinorTicks = true;
	/**
	 * plot tick line
	 */
	public boolean _plotLine = true;
	/**
	 * Thickness of lines
	 */
	public int _thickness = 1;
	/**
	 *
	 */
	public Color _color = Color.black;

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TickLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga)
	{
		super.copyInto(ga);
		if(ga instanceof TickLineAttr)
		{
			TickLineAttr lea = (TickLineAttr) ga;
			lea._position = this._position;
			lea._tickLocation = this._tickLocation;
			lea._percentMajorTickLength = this._percentMajorTickLength;
			lea._percentMinorTickLength = this._percentMinorTickLength;
			lea._plotMajorTicks = this._plotMajorTicks;
			lea._plotMinorTicks = this._plotMinorTicks;
			lea._color = this._color;
		}
	}

}

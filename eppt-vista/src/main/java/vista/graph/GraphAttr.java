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
 * Attributes of a graph.
 *
 * @author Nicky Sandhu
 * @version $Id: GraphAttr.java,v 1.1 2003/10/02 20:49:00 redwood Exp $
 */
public class GraphAttr extends GEAttr
{
	/**
	 * attributes for the title
	 */
	private TextLineAttr _tla = new TextLineAttr();
	/**
	 * attributes for the plot
	 */
	private PlotAttr _pa = new PlotAttr();
	/**
	 * attributes for the legend
	 */
	private GEAttr _la = new LegendAttr();

	/**
	 * sets up default color table.
	 */
	public static Color[] getDefaultColorTable()
	{
		Color[] colorTable = {Color.red, Color.green, Color.blue, Color.pink,
				Color.cyan, Color.orange, Color.black, Color.magenta,
				Color.yellow, Color.white};
		return colorTable;
	}

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga)
	{
		super.copyInto(ga);
		if(ga instanceof GraphAttr)
		{
			GraphAttr tla = (GraphAttr) ga;
		}
	}

	/**
	 * gets plot attributes
	 */
	public PlotAttr getPlotAttributes()
	{
		return _pa;
	}

	/**
	 * sets plot attributes
	 */
	public void setPlotAttributes(PlotAttr pa)
	{
		_pa = pa;
	}

	/**
	 * gets legend attributes
	 */
	public GEAttr getLegendAttributes()
	{
		return _la;
	}

	/**
	 * sets legend attributes
	 */
	public void setLegendAttributes(GEAttr la)
	{
		_la = la;
	}

	/**
	 * gets title attributes
	 */
	public TextLineAttr getTitleAttributes()
	{
		return _tla;
	}

	/**
	 * sets title attributes
	 */
	public void setTitleAttributes(TextLineAttr tla)
	{
		_tla = tla;
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Color;

/**
 * Attributes of a graph.
 * 
 * @author Nicky Sandhu
 * @version $Id: GraphAttr.java,v 1.1 2003/10/02 20:49:00 redwood Exp $
 */
public class GraphAttr extends GEAttr {
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
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof GraphAttr) {
			GraphAttr tla = (GraphAttr) ga;
		}
	}

	/**
	 * sets plot attributes
	 */
	public void setPlotAttributes(PlotAttr pa) {
		_pa = pa;
	}

	/**
	 * gets plot attributes
	 */
	public PlotAttr getPlotAttributes() {
		return _pa;
	}

	/**
	 * sets legend attributes
	 */
	public void setLegendAttributes(GEAttr la) {
		_la = la;
	}

	/**
	 * gets legend attributes
	 */
	public GEAttr getLegendAttributes() {
		return _la;
	}

	/**
	 * sets title attributes
	 */
	public void setTitleAttributes(TextLineAttr tla) {
		_tla = tla;
	}

	/**
	 * gets title attributes
	 */
	public TextLineAttr getTitleAttributes() {
		return _tla;
	}

	/**
	 * sets up default color table.
	 */
	public static Color[] getDefaultColorTable() {
		Color[] colorTable = { Color.red, Color.green, Color.blue, Color.pink,
				Color.cyan, Color.orange, Color.black, Color.magenta,
				Color.yellow, Color.white };
		return colorTable;
	}
}

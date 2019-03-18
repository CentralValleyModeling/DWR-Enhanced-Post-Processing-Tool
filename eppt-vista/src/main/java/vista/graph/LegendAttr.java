/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;


/**
 * Attributes of a Plot
 * 
 * @see Plot
 * @author Nicky Sandhu (DWR).
 * @version $Id: LegendAttr.java,v 1.1 2003/10/02 20:49:04 redwood Exp $
 */
public class LegendAttr extends LegendItemAttr {
	/**
	 * Marks the boundaries of the text with ticks.
	 */
	public boolean _boundariesMarked = false;
	/**
	 * Marks the title of the legend
	 */
	public boolean _legendHeader = false;

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof LegendAttr) {
			LegendAttr tla = (LegendAttr) ga;
		}
	}
}

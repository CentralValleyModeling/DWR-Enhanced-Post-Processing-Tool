/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;


/**
 * Attributes of a TickText
 * 
 * @see TickText
 * @author Nicky Sandhu (DWR).
 * @version $Id: TickTextAttr.java,v 1.1 2003/10/02 20:49:11 redwood Exp $
 */
public class TickTextAttr extends TextLineAttr {
	/**
	 * Text is centered on tick value
	 */
	public boolean _centeredOnBounds = true;
	/**
	 * Marks the boundaries of the text with ticks.
	 */
	public boolean _boundariesMarked = false;

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof TickTextAttr) {
			TickTextAttr tla = (TickTextAttr) ga;
			tla._boundariesMarked = this._boundariesMarked;
			tla._centeredOnBounds = this._centeredOnBounds;
		}
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;


/**
 * Attributes of a LegendItem
 * 
 * @see LegendItem
 * @author Nicky Sandhu (DWR).
 * @version $Id: LegendItemAttr.java,v 1.1 2003/10/02 20:49:04 redwood Exp $
 */
public class LegendItemAttr extends TextLineAttr {
	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof LegendItemAttr) {
			LegendItemAttr tla = (LegendItemAttr) ga;
			tla._lea = this._lea;
		}
	}

	/**
   *
   */
	public LineElementAttr getLineElementAttributes() {
		return _lea;
	}

	/**
   *
   */
	public void setLineElementAttributes(LineElementAttr lea) {
		_lea = lea;
	}

	/**
   *
   */
	private LineElementAttr _lea = new LineElementAttr();
}

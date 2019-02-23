/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;


/**
 * Attributes of a Axis element
 * 
 * @see Axis
 * @author Nicky Sandhu (DWR).
 * @version $Id: AxisAttr.java,v 1.1 2003/10/02 20:48:48 redwood Exp $
 */
public class AxisAttr extends TickLineAttr {
	/**
	 * plot major tick labels?
	 */
	public boolean _plotMajorTickLabels = true;
	/**
	 * plot axis label?
	 */
	public boolean _plotAxisLabel = true;

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof AxisAttr) {
			AxisAttr tla = (AxisAttr) ga;
			tla._plotMajorTickLabels = this._plotMajorTickLabels;
			tla._plotAxisLabel = this._plotAxisLabel;
			tla._ttxa = this._ttxa;
		}
	}

	/**
	 * get the attributes of the major tick label element
	 */
	public TickTextAttr getTickTextAttributes() {
		return _ttxa;
	}

	/**
	 * set the attributes of the labels for the major ticks
	 */
	public void getTickTextAttributes(TickTextAttr ttxa) {
		_ttxa = ttxa;
	}

	/**
	 * The major tick label TickText attributes
	 */
	private TickTextAttr _ttxa = new TickTextAttr();
}

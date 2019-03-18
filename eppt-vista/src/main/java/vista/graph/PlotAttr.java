/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Font;

/**
 * Attributes of a Plot
 * 
 * @see Plot
 * @author Nicky Sandhu (DWR).
 * @version $Id: PlotAttr.java,v 1.1 2003/10/02 20:49:06 redwood Exp $
 */
public class PlotAttr extends GEAttr {
	/**
   *
   */
	public Font _font = TextLineAttr._fontTable[(int) (TextLineAttr._fontTable.length * 0.75)];
	/**
	 * original font when first created.
	 */
	public int _originalFontSize = _font.getSize();
	/**
	 * draw border around drawing area.
	 */
	public boolean _drawBorder = true;

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * TextLineAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof PlotAttr) {
			PlotAttr tla = (PlotAttr) ga;
		}
	}

	/**
   *
   */
	public void setTickLineAttr(TickLineAttr attr) {
		_tla = attr;
	}

	/**
   *
   */
	public TickLineAttr getTickLineAttr() {
		return _tla;
	}

	/**
   *
   */
	public void setTickTextAttr(TickTextAttr attr) {
		_ttxa = attr;
	}

	/**
   *
   */
	public TickTextAttr getTickTextAttr() {
		return _ttxa;
	}

	/**
   *
   */
	public void setTextLineAttr(TextLineAttr attr) {
		_txla = attr;
	}

	/**
   *
   */
	public TextLineAttr getTextLineAttr() {
		return _txla;
	}

	/**
   *
   */
	private TickLineAttr _tla = new TickLineAttr();
	/**
   *
   */
	private TickTextAttr _ttxa = new TickTextAttr();
	/**
   *
   */
	private TextLineAttr _txla = new TextLineAttr();

}

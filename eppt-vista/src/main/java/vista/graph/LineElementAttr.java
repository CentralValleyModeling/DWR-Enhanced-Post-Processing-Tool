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
 * Attributes for the line elements
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: LineElementAttr.java,v 1.1 2003/10/02 20:49:05 redwood Exp $
 */
public class LineElementAttr extends GEAttr {
	/**
	 * line style
	 */
	public static final int PLAIN = 1;
	/**
	 * line style
	 */
	public static final int DASHED = PLAIN + 1;
	/**
	 * line style
	 */
	public static final int DOTTED = DASHED + 1;
	/**
	 * thickness of line in pixels
	 */
	public int _thickness = 1;
	/**
	 * style of line such as plain, dashed, dotted, etcetra.
	 */
	public int _style = PLAIN;

	/**
	 * sets Thickness
	 */
	public void setThickness(int thickness) {
		_thickness = thickness;
	}

	/**
	 * gets Thickness
	 */
	public int getThickness() {
		return _thickness;
	}

	/**
	 * sets Style
	 */
	public void setStyle(int style) {
		_style = style;
	}

	/**
	 * copies the fields into the given GEAttr object. Also copies in the
	 * LineElementAttr if the object is of that type.
	 */
	public void copyInto(GEAttr ga) {
		super.copyInto(ga);
		if (ga instanceof LineElementAttr) {
			LineElementAttr lea = (LineElementAttr) ga;
			lea._thickness = this._thickness;
			lea._style = this._style;
		}
	}

}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * A circle symbol
 * 
 * @author Nicky Sandhu
 * @version $Id: CircleSymbol.java,v 1.1 2003/10/02 20:48:50 redwood Exp $
 */
public class CircleSymbol extends Symbol {
	/**
	 * for debugging
	 */
	public static final boolean DEBUG = false;

	/**
	 * Constructor.
	 */
	public CircleSymbol(SymbolAttr attributes) {
		super(attributes);
		setName("CircleSymbol");
	}

	/**
	 * true if x,y hits the drawing. More precise than contains(x,y)
	 */
	public boolean hitsDrawing(int x, int y) {
		Rectangle r = getInsetedBounds();
		int wd = r.width / 4;
		int ht = r.height / 4;
		int radius = Math.min(wd, ht);
		int xc = r.x - radius / 2;
		int yc = r.y - radius / 2;
		return Math.sqrt((x - r.x) * (x - r.x) + (y - r.y) * (y - r.y)) <= radius / 2;
	}

	/**
	 * draws polygon representing symbol. Currently for efficiency no scaling is
	 * done. However a subclass will do so. In this way it would be the users
	 * choice to use the most suitable class.
	 */
	public void Draw() {
		SymbolAttr attr = (SymbolAttr) getAttributes();
		Graphics gc = getGraphics();

		Rectangle r = getInsetedBounds();
		int wd = r.width / 4;
		int ht = r.height / 4;
		int radius = Math.min(wd, ht);
		int x = r.x - radius / 2;
		int y = r.y - radius / 2;
		if (attr._isFilled)
			gc.fillArc(x, y, radius, radius, 0, 360);
		else
			gc.drawArc(x, y, radius, radius, 0, 360);
	}

	/**
	 * calculates the preferred size of this element
	 * 
	 * @return The preferred size
	 */
	public Dimension getPreferredSize() {
		return new Dimension(25, 25);
	}

	/**
	 * calculates the minimum size of this element
	 * 
	 * @return The minimum size
	 */
	public Dimension getMinimumSize() {
		return getPreferredSize();
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + getName() + ".";
	}
}

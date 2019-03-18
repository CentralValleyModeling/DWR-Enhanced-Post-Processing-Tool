/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.util.Properties;

/**
 * Symbol is a polygon shape drawn with respect to the x and y coordinates of
 * rectangle. No scaling is currently done.
 * <p>
 * The symbol is a polygon and can be represented by a series of x and y
 * coordinates. The symbol may be filled or outlined. The x and y coordinates
 * are taken to be in units of pixels ~ 1/72 inch for most displays.
 *
 * @author Nicky Sandhu
 * @version $Id: Symbol.java,v 1.1 2003/10/02 20:49:08 redwood Exp $
 */
public class Symbol extends GraphicElement
{
	/**
	 * for debugging
	 */
	public static final boolean DEBUG = false;
	private Polygon _sp = new Polygon();

	/**
	 * Constructor.
	 */
	public Symbol()
	{
		this(new SymbolAttr());
	}

	/**
	 * Constructor.
	 */
	public Symbol(SymbolAttr attributes)
	{
		super(attributes);
		setName("Symbol");
	}

	/**
	 * true if x,y hits the drawing. More precise than contains(x,y)
	 */
	public boolean hitsDrawing(int x, int y)
	{
		Rectangle r = getInsetedBounds();
		_sp.translate(r.x, r.y);
		boolean drawingHit = _sp != null && _sp.contains(x, y);
		_sp.translate(-r.x, -r.y);
		return drawingHit;
	}

	/**
	 * draws polygon representing symbol. Currently for efficiency no scaling is
	 * done. However a subclass will do so. In this way it would be the users
	 * choice to use the most suitable class.
	 */
	public void Draw()
	{
		SymbolAttr attr = (SymbolAttr) getAttributes();
		Graphics gc = getGraphics();

		Rectangle r = getInsetedBounds();
		_sp = attr.getSymbol();
		_sp.translate(r.x, r.y);

		if(attr._isFilled)
		{
			getGraphics().fillPolygon(_sp);
		}
		else
		{
			getGraphics().drawPolygon(_sp);
		}

		_sp.translate(-r.x, -r.y);
	}

	/**
	 * calculates the preferred size of this element
	 *
	 * @return The preferred size
	 */
	public Dimension getPreferredSize()
	{
		return new Dimension(25, 25);
	}

	/**
	 * calculates the minimum size of this element
	 *
	 * @return The minimum size
	 */
	public Dimension getMinimumSize()
	{
		return getPreferredSize();
	}

	/**
	 *
	 */
	public String getPrefixTag(String prefixTag)
	{
		return prefixTag + getName() + ".";
	}

	/**
	 *
	 */
	public void toProperties(Properties p, String prefixTag)
	{
		super.toProperties(p, getPrefixTag(prefixTag));
	}

	/**
	 *
	 */
	public void fromProperties(Properties p, String prefixTag)
	{
		super.fromProperties(p, getPrefixTag(prefixTag));
	}
}

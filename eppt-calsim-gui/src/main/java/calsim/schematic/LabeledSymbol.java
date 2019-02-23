/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.schematic;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

import vista.graph.GraphicElement;
import vista.graph.ScaledElement;
import vista.graph.Symbol;
import vista.graph.TextLine;

//import java.util.Properties;

/**
 * Symbol is a polygon shape drawn with respect to the x and y coordinates
 * of rectangle. No scaling is currently done.<p>
 * The symbol is a polygon and can be represented by a series of x and y coordinates.
 * The symbol may be filled or outlined. The x and y coordinates are taken to be
 * in units of pixels ~ 1/72 inch for most displays.
 *
 * @author Nicky Sandhu
 * @version $Id: LabeledSymbol.java,v 1.1.2.3 1999/06/01 16:58:44 nsandhu Exp $
 */
public class LabeledSymbol extends GraphicElement implements ScaledElement
{
	/**
	 * for debugging
	 */
	public static final boolean DEBUG = false;
	private static Rectangle CONST_RECT = new Rectangle(0, 0, 80, 80);
	private TextLine _label;
	private Symbol _sym;

	/**
	 * Constructor.
	 */
	public LabeledSymbol(String label, Symbol sym)
	{
		_label = new TextLine(label);
		_sym = sym;
		setName("Labeled Symbol");
	}

	/**
	 * gets the element containing the label
	 */
	public TextLine getTextLine()
	{
		return _label;
	}

	/**
	 *
	 */
	public Symbol getSymbol()
	{
		return _sym;
	}

	/**
	 * true if x,y hits the drawing. More precise than contains(x,y)
	 */
	public boolean hitsDrawing(int x, int y)
	{
		if(_sym != null)
		{
			return _sym.hitsDrawing(x, y);
		}
		else
		{
			return false;
		}
	}

	/**
	 * draws polygon representing symbol. Currently for efficiency no
	 * scaling is done. However a subclass will do so. In this way it
	 * would be the users choice to use the most suitable class.
	 */
	public void Draw()
	{
		Graphics gc = getGraphics();
		Rectangle r = getInsetedBounds();
		//
		CONST_RECT.x = r.x;
		CONST_RECT.y = r.y;
		_sym.draw(gc, CONST_RECT);
		// draw label at center of symbol
		int oh = r.height;
		int ow = r.width;
		r.height = 0;
		r.width = 0;
		_label.draw(gc, r);
		r.height = oh;
		r.width = ow;
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
		return _sym.getPreferredSize();
	}

	/**
	 * prefix tag for storing attributes
	 */
	public String getPrefixTag(String prefixTag)
	{
		return prefixTag + getName() + ".";
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Rectangle;

/**
 * This controls the drawing of a line within the bounds of the given rectangle.
 * <p>
 * Currently this class only draws horizontal and vertical lines clipped within
 * the drawing region. Furthermore each line is drawn from a starting point to
 * the end of the drawing Area dimensions.
 * <p>
 * Also grid are drawn for each axis by the Plot object. This further means that
 * the lines for grid on opposing axis must match up. This can be done by making
 * sure that both axes share the same major tick marks. This is something that
 * needs to be done inside Plot object
 *
 * @author Nicky Sandhu
 * @version $Id: Grid.java,v 1.1 2003/10/02 20:49:01 redwood Exp $
 * @see Axis
 * @see Plot
 */
public class Grid extends GraphicElement
{

	/**
	 * for debugging
	 */
	public static final boolean DEBUG = false;
	/**
	 * The major tick marks
	 */
	private TickData _tickData;
	private Axis _axis;

	/**
	 * Constructor initializes the attributes of this element.
	 */
	public Grid(Axis axis)
	{
		this(new GridAttr(), axis);
	}

	/**
	 * Constructor initializes the attributes of this element.
	 */
	public Grid(GridAttr attributes, Axis axis)
	{
		super(attributes);
		_axis = axis;

	}

	/**
	 * draws horizontal or vertical line with the major tick marks as references
	 */
	public void Draw()
	{
		GridAttr ga = (GridAttr) getAttributes();
		Rectangle drawingArea = getDrawBounds();
		Color prevColor = getGraphics().getColor();
		getGraphics().setColor(ga._color);
		_tickData = _axis.getMajorTickData();
		int n = _tickData.getNumber();
		if(ga._orientation == GEAttr.HORIZONTAL)
		{
			for(int i = 1; i < n - 1; i++)
			{
				int y = _tickData.getUCValue(i);
				getGraphics().drawLine(drawingArea.x, y,
						drawingArea.x + drawingArea.width, y);
			}
		}
		else if(ga._orientation == GEAttr.VERTICAL)
		{
			for(int i = 1; i < n - 1; i++)
			{
				int x = _tickData.getUCValue(i);
				getGraphics().drawLine(x, drawingArea.y, x,
						drawingArea.y + drawingArea.height);
			}
		}

		getGraphics().setColor(prevColor);
	}

	/**
	 * Calculates the preferred size of this element
	 */
	public Dimension getPreferredSize()
	{
		GEAttr attr = getAttributes();
		if(attr._orientation == GEAttr.HORIZONTAL)
		{
			return new Dimension(25, 10);
		}

		return new Dimension(10, 25);
	}

	/**
	 * Calculates the minimum size of this element
	 */
	public Dimension getMinimumSize()
	{
		return getPreferredSize();
	}
}

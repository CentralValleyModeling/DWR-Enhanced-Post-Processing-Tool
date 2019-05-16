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

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * A line which represents the curve characterstics.
 *
 * @author Nicky Sandhu
 * @version $Id: CurveLegendLine.java,v 1.1 2003/10/02 20:48:52 redwood Exp $
 */
public class CurveLegendLine extends GraphicElement
{
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	/**
	 * The curve which is being represented
	 */
	protected Curve _curve;

	/**
	 * A line with the same attributes as the curve
	 */
	public CurveLegendLine(Curve curve)
	{
		super(curve.getAttributes());
		_curve = curve;
	}

	/**
	 *
	 */
	public Dimension getPreferredSize()
	{
		return new Dimension(50, 20);
	}

	/**
	 *
	 */
	public Dimension getMinimumSize()
	{
		return getPreferredSize();
	}

	/**
	 *
	 */
	public void Draw()
	{
		Graphics gc = getGraphics();
		CurveAttr attr = (CurveAttr) getAttributes();
		Rectangle r = getBounds();
		int yPos = r.y + r.height / 2;
		if(DEBUG)
		{
			System.out.println("Rectangle for legend item: " + r);
		}
		if(attr._thickness > 1)
		{
			GraphUtils.drawThickLine(gc, r.x, yPos, r.x + r.width, yPos,
					attr._thickness);
		}
		else
		{
			gc.drawLine(r.x, yPos, r.x + r.width, yPos);
		}
		if(attr._drawSymbol)
		{
			Symbol sym = _curve.getSymbol();
			if(sym != null)
			{
				Rectangle symbolBounds = sym.getBounds();
				if(DEBUG)
				{
					System.out.println("Symbol for legend item: "
							+ symbolBounds);
				}
				symbolBounds.x = r.x + r.width / 2;
				symbolBounds.y = yPos;
				sym.draw(gc, symbolBounds);
				symbolBounds.x = r.x + r.width / 4;
				symbolBounds.y = yPos;
				sym.draw(gc, symbolBounds);
				symbolBounds.x = r.x + (r.width * 3) / 4;
				symbolBounds.y = yPos;
				sym.draw(gc, symbolBounds);
			}
		}
	}
}

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
import java.awt.Font;
import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.Properties;

/**
 * This is an aggregate of LegendItem(s). The layout is handled by the
 * GELineLayout
 *
 * @author Nicky Sandhu
 * @version $Id: Legend.java,v 1.1 2003/10/02 20:49:03 redwood Exp $
 * @see LegendItem
 * @see GELineLayout
 */
public class Legend extends GEContainer implements ScaledElement
{
	boolean _ignoreBounds = false;

	/**
	 * constructor
	 */
	public Legend()
	{
		this(new LegendAttr());
	}

	/**
	 * constructor
	 */
	public Legend(LegendAttr attributes)
	{
		super(attributes);
		setLayout(new GELineLayout(GELineLayout.VERTICAL,
				GELineLayout.CENTERED_BETWEEN_BOUNDS));
	}

	/**
	 * adds a graphic element to the legend. Only LegendItems are acceptable
	 * additions to this container.
	 *
	 * @see LegendItem
	 */
	public void add(GraphicElement ge)
	{
		if(ge instanceof LegendItem)
		{
			LegendItem li = (LegendItem) ge;
			li.setParent(this);
			LegendItemAttr lia = (LegendItemAttr) li.getAttributes();
			lia._font = ((LegendItemAttr) getAttributes())._font;
			super.add(ge);
			li.setName(li.getLegendName().trim());
		}
		else
		{
			super.add(ge);
		}
	}

	/**
	 * draws a rectangle around legend and then draws its components
	 */
	public void Draw()
	{
		LegendAttr attr = (LegendAttr) getAttributes();
		super.Draw();

		if(attr._boundariesMarked)
		{
			Rectangle drawingArea = getDrawBounds();
			getGraphics().drawRect(drawingArea.x, drawingArea.y,
					drawingArea.width, drawingArea.height);
		}
	}

	/**
	 *
	 */
	public void setFontSize(int size)
	{
		Font font = null;
		for(Enumeration iterator = getIterator(); iterator.hasMoreElements(); )
		{
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			if(ge instanceof LegendItem)
			{
				LegendItemAttr lia = (LegendItemAttr) ge.getAttributes();
				lia._resizeOnTheFly = false;
				if(font == null)
				{
					font = lia._font;
					font = new Font(font.getName(), font.getStyle(), size);
				}
				lia._font = font;
			}
		}
	}

	/**
	 * this layout can ignore its size when laying out its children and can lay
	 * them out depending upon their preferred sizes.
	 */
	public void doLayout()
	{
		if(_ignoreBounds)
		{
			Rectangle r = getBounds();
			Dimension d = getPreferredSize();
			r.width = d.width;
			r.height = d.height / 2; // the font's return too much space in
			// preferred
			super.doLayout();
		}
		else
		{
			super.doLayout();
		}
	}

	/**
	 * Sets font for all the components for which it can be set.
	 */
	public void setFont(Font font)
	{
		for(Enumeration iterator = getIterator(); iterator.hasMoreElements(); )
		{
			GraphicElement ge = (GraphicElement) iterator.nextElement();
			if(ge instanceof LegendItem)
			{
				LegendItemAttr lia = (LegendItemAttr) ge.getAttributes();
				lia._font = font;
			}
		}
	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 *
	 * @param prefixTag A tag to assign the context for these properties e.g. if these
	 *                  properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag)
	{
		super.toProperties(p, getPrefixTag(prefixTag));
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag)
	{
		super.fromProperties(p, getPrefixTag(prefixTag));
	}

	/**
	 *
	 */
	public String getPrefixTag(String prefixTag)
	{
		return prefixTag + getName() + "Legend.";
	}
}

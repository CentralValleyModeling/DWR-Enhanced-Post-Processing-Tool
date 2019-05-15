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

import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.Enumeration;
import java.util.Properties;

/**
 * A class handling TextLine objects which share common data values. The
 * GELineLayoutManager is used for managing layout. Also these labels are given
 * equal space along the linear length such that they match up with the TickText
 * major tick marks.
 * <p>
 * 
 * Attributes for all TextLine objects such as font, background color,
 * justifcation are in the attribute object. This allows the TextLine objects to
 * share such data in an external object. (Flyweight sharing pattern)
 * 
 * @see TextLineAttr
 * @author Nicky Sandhu
 * @version $Id: TickText.java,v 1.1 2003/10/02 20:49:11 redwood Exp $
 */
public class TickText extends GEContainer implements FontResizable {
	/**
	 * creates a container for text sharing common attributes with a row layout
	 * manager.
	 * 
	 * @see GERowLayoutManager
	 */
	public TickText(String[] labels, TickData tickData) {
		this(new TickTextAttr(), labels, tickData);
	}

	/**
	 * creates a container for text sharing common attributes with a row layout
	 * manager.
	 * 
	 * @see GERowLayoutManager
	 */
	public TickText(TickTextAttr attributes, String[] labels, TickData tickData) {

		super(attributes);
		setLayout(new GETickLayout(GETickLayout.HORIZONTAL,
				GETickLayout.CENTERED_ON_BOUNDS, tickData));

		if (labels != null)
			setLabels(labels);
	}

	/**
	 * sets the tick labels
	 */
	public void setLabels(String[] labels) {
		removeAll();

		for (int i = 0; i < labels.length; i++) {
			TextLine tl = new TextLine((TextLineAttr) getAttributes(),
					labels[i]);
			this.add(tl);
		}

	}

	/**
	 * calls on it super class to draw all its components. Also has the ability
	 * of marking the major tick marks between labels.
	 */
	public void Draw() {
		super.Draw();
		TickTextAttr attr = (TickTextAttr) getAttributes();
		Graphics gc = getGraphics();
		if (attr._boundariesMarked) {
			GELayoutManager layout = getLayout();
			if (layout instanceof GETickLayout) {
				for (Enumeration iterator = getDrawIterator(); iterator
						.hasMoreElements();) {
					GraphicElement ge = (GraphicElement) iterator.nextElement();
					Rectangle r = ge.getInsetedBounds();
					if (((GETickLayout) layout).getOrientation() == GETickLayout.HORIZONTAL)
						gc.drawLine(r.x, r.y, r.x, r.y + r.height);
					else {
						gc.drawLine(r.x, r.y, r.x + r.width, r.y);
					}
				} // iterator loop
			} // if tick layout
		}
	}

	/**
    *
    */
	public void setFontSize(int size) {
		TickTextAttr attr = (TickTextAttr) getAttributes();
		Font font = attr._font;
		attr._font = new Font(font.getName(), font.getStyle(), size);
		attr._resizeOnTheFly = false;
	}

	/**
	 * sets font by ratio.
	 */
	public void setFontByRatio(double fontResizeRatio) {
		TickTextAttr attr = (TickTextAttr) getAttributes();
		if (attr._resizeOnTheFly == false)
			return;
		int newFontSize = (int) (attr._originalFontSize * fontResizeRatio);
		if (newFontSize < 0)
			newFontSize = 0;
		if (newFontSize > TextLineAttr._fontTable.length - 1)
			newFontSize = TextLineAttr._fontTable.length - 1;
		attr._font = TextLineAttr._fontTable[newFontSize];

	}

	/**
	 * Returns its properties in a Properties object. These are saved on disk.
	 * 
	 * @param prefixTag
	 *            A tag to assign the context for these properties e.g. if these
	 *            properties belong to Axis class then prefixTag will be "Axis."
	 */
	public void toProperties(Properties p, String prefixTag) {
		String localTag = getPrefixTag(prefixTag);
		if (p == null)
			return;
		TickTextAttr attr = (TickTextAttr) getAttributes();

		p.put(localTag + "centeredOnBounds",
				new Boolean(attr._centeredOnBounds).toString());
		p.put(localTag + "boundariesMarked",
				new Boolean(attr._boundariesMarked).toString());

		super.toProperties(p, localTag);
	}

	/**
   *
   */
	public String getPrefixTag(String prefixTag) {
		return prefixTag + getName() + "TickText.";
	}

	/**
	 * initializes attributes and state from Properties object.
	 */
	public void fromProperties(Properties p, String prefixTag) {
		super.fromProperties(p, getPrefixTag(prefixTag));
	}

}

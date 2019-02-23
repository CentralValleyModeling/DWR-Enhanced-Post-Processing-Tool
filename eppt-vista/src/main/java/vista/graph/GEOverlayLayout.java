/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;
import java.awt.Insets;
import java.awt.Rectangle;

/**
 * Lays out Bounded(s) on top of each other.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: GEOverlayLayout.java,v 1.1 2003/10/02 20:48:58 redwood Exp $
 */
public class GEOverlayLayout implements GELayoutManager
{
	/**
	 * debuggin' purposes
	 */
	public static final boolean DEBUG = false;

	/**
	 * constructor
	 */
	public GEOverlayLayout()
	{
	}

	/**
	 * Adds the specified element with the specified name to the layout.
	 *
	 * @param name the element name
	 * @param comp the element to be added
	 */
	public void addLayoutElement(Object obj, Bounded comp)
	{
	}

	/**
	 * Removes the specified element from the layout.
	 *
	 * @param comp the element ot be removed
	 */
	public void removeLayoutElement(Bounded comp)
	{
	}

	/**
	 * Calculates the preferred size dimensions for the specified panel given
	 * the Boundeds in the specified parent BoundedComposite.
	 *
	 * @param parent the Bounded to be laid out
	 * @see #minimumLayoutSize
	 */
	public Dimension preferredLayoutSize(BoundedComposite parent)
	{
		int n = parent.getElementCount();
		Insets insets = parent.getInsets();

		int pWidth = 0;
		int pHeight = 0;
		Bounded ge;

		for(int i = 0; i < n; i++)
		{
			ge = parent.getElement(i);
			Dimension d = ge.getPreferredSize();
			pWidth = Math.max(pWidth, d.width);
			pHeight = Math.max(pHeight, d.height);
		}
		return new Dimension(pWidth + (insets.left + insets.right), pHeight
				+ (insets.top + insets.bottom));
	}

	/**
	 * Calculates the minimum size dimensions for the specified panel given the
	 * Boundeds in the specified parent BoundedComposite.
	 *
	 * @param parent the Bounded to be laid out
	 * @see #preferredLayoutSize
	 */
	public Dimension minimumLayoutSize(BoundedComposite parent)
	{
		return preferredLayoutSize(parent);
	}

	/**
	 * Lays out the BoundedComposite in the specified panel. Lays out all the
	 * elements in a single row with the space equally divided amongst the
	 * elements.
	 *
	 * @param parent the Bounded which needs to be laid out
	 */
	public void layoutContainer(BoundedComposite parent)
	{
		int n = parent.getElementCount();

		Bounded ge = null;

		Rectangle parentBounds = parent.getInsetedBounds();
		Rectangle geBounds = new Rectangle(parentBounds);

		for(int i = 0; i < n; i++)
		{
			ge = parent.getElement(i);
			ge.setBounds(geBounds);
		}
	}
}

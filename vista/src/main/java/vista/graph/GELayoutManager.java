/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;

/**
 * This interface provides methods for managing the layouts of BoundedComposite
 * objects which contain other Bounded objects ( which may themselves be
 * Composites). This would be the "Strategy" Pattern. This strategy object
 * encapsulates the know how of laying out composites.
 * <p>
 * <p>
 * It is the responsibility of classses implementing this interface to query
 * Bounded elements for their preferred sizes and lay them out accordingly. The
 * GEContainer classes use this interface for laying out their children.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: GELayoutManager.java,v 1.1 2003/10/02 20:48:57 redwood Exp $
 * @see Bounded
 * @see BoundedComposite
 * @see GEContainer
 */
public interface GELayoutManager
{
	/**
	 * Adds the specified element with the specified name to the layout.
	 *
	 * @param name    the element name
	 * @param element the element to be added
	 */
	void addLayoutElement(Object obj, Bounded element);

	/**
	 * Removes the specified element from the layout.
	 *
	 * @param element the element ot be removed
	 */
	void removeLayoutElement(Bounded element);

	/**
	 * Calculates the preferred size dimensions for the specified panel given
	 * the graphicElements in the specified parent BoundedComposite.
	 *
	 * @param parent the Bounded to be laid out
	 * @see #minimumLayoutSize
	 */
	Dimension preferredLayoutSize(BoundedComposite parent);

	/**
	 * Calculates the minimum size dimensions for the specified panel given the
	 * graphicElements in the specified parent BoundedComposite.
	 *
	 * @param parent the Bounded to be laid out
	 * @see #preferredLayoutSize
	 */
	Dimension minimumLayoutSize(BoundedComposite parent);

	/**
	 * Lays out the BoundedComposite in the specified panel.
	 *
	 * @param parent the Bounded which needs to be laid out
	 */
	void layoutContainer(BoundedComposite parent);
}

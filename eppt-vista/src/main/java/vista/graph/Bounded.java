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
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;

/**
 * This interface is implemented by any object that has bounds defined by a
 * rectangular region. Also this object should be able to be queried for its
 * preferred/minimum dimensions.
 * <p>
 * This interface is primarily in this package for purposes of interacting with
 * layout managers and other interfaces that require only the bounds information
 * of a graphic element.
 * <p>
 *
 * @author Nicky Sandhu
 * @version $Id: Bounded.java,v 1.1 2003/10/02 20:48:49 redwood Exp $
 * @see LayoutManager
 * @see GraphicElement
 */
public interface Bounded
{
	/**
	 * gets this element's bounds
	 *
	 * @return the minimum rectangular region enclosing this object
	 */
	Rectangle getBounds();

	/**
	 * set this element's bounds
	 *
	 * @param
	 */
	void setBounds(Rectangle r);

	/**
	 * calculates the preferred size of this element
	 *
	 * @return the preferred size
	 */
	Dimension getPreferredSize();

	/**
	 * calculates the minimum size of this element
	 *
	 * @return the minimum size
	 */
	Dimension getMinimumSize();

	/**
	 * gets the elements size, i.e. the width and height
	 *
	 * @return a new Dimension object with the element size
	 */
	Dimension getSize();

	/**
	 * sets size of this element.
	 *
	 * @param d The dimension to which the element is to be set.
	 */
	void setSize(Dimension d);

	/**
	 * get bounds of rectangle after allowing for insets
	 *
	 * @return the rectangular region after accounting for insets
	 */
	Rectangle getInsetedBounds();

	/**
	 * The insets for this element
	 *
	 * @return The Insets object.
	 */
	Insets getInsets();

	/**
	 * Sets the insets for this element
	 *
	 * @param i The insets for this bounded object
	 */
	void setInsets(Insets i);

	/**
	 * checks to see if point is contained with element dimensions
	 *
	 * @param p The point for which containment is to be checked
	 * @return true if point lies within bounds
	 */
	boolean contains(Point p);

	/**
	 * checks to see if point is contained with element dimensions
	 *
	 * @param x The x co-ordinate of the point
	 * @param y The y co-ordinate of the point
	 * @return true if point lies within bounds
	 */
	boolean contains(int x, int y);

	/**
	 * Checks if two rectangles intersect.
	 *
	 * @param r The rectangle with which intersection of this object is to be
	 *          checked
	 * @return true if rectangle intersects with this object.
	 */
	boolean intersects(Rectangle r);
}

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


/**
 * This is an interface to a composite of drawable elements. A drawing iterator
 * should be implemented to define the order of drawing.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: DrawableComposite.java,v 1.1 2003/10/02 20:48:54 redwood Exp $
 */
public interface DrawableComposite extends Drawable
{
	/**
	 * reorders element container array to place the given element at the first
	 * index. This has the effect of drawing this element before others.
	 *
	 * @param ge The graphic element to place at the beginning.
	 */
	void drawFirst(Drawable ge);

	/**
	 * reorders element container array to place the given element at the last
	 * index. This has the effect of drawing this element after others.
	 *
	 * @param ge The graphic element to place at the beginning.
	 */
	void drawLast(Drawable ge);

	/**
	 * get the iterator used to specify drawing order
	 */
	DrawIterator getDrawIterator();

	/**
	 * set the iterator to be used to specify drawing order.
	 */
	void setDrawIterator(DrawIterator iterator);
}

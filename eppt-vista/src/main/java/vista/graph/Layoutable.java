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
 * This interface defines the layoutable object.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: Layoutable.java,v 1.1 2003/10/02 20:49:03 redwood Exp $
 */
public interface Layoutable
{
	/**
	 * gets current layout manager
	 *
	 * @returns GELayoutManager object
	 */
	GELayoutManager getLayout();

	/**
	 * sets layout manager
	 */
	void setLayout(GELayoutManager lm);

	/**
	 * instructs the layout manager to layout the child elements in accordance
	 * with their preferred sizes.
	 */
	void doLayout();
}

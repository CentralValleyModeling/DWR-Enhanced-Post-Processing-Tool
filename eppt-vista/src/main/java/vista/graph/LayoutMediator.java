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

/**
 * Mediates between layout managers to synchronize or exchange layout
 * information
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: LayoutMediator.java,v 1.1 2003/10/02 20:49:03 redwood Exp $
 */
public interface LayoutMediator
{
	/**
	 * Add layout manager to the group to be synchronized
	 *
	 * @param lm The layout manager to be added
	 */
	void addLayoutManager(GELayoutManager lm);

	/**
	 * Get the preferred size by getting information from layout managers.
	 */
	Dimension getPreferredSize(Object object);

	/**
	 * Get the minimum size by getting information from layout managers.
	 */
	Dimension getMinimumSize(Object object);
}

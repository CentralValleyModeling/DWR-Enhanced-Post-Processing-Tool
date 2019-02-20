/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

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
import java.util.ArrayList;

/**
 * Mediates between layout managers to synchronize or exchange layout
 * information
 * 
 * @author Nicky Sandhu (DWR).
 * @version $Id: GEBorderLayoutMediator.java,v 1.1 2003/10/02 20:48:56 redwood
 *          Exp $
 */
public class GEBorderLayoutMediator implements LayoutMediator {
	/**
	 * for debuggin'
	 */
	public static final boolean DEBUG = false;

	/**
	 * Constructor
	 */
	public GEBorderLayoutMediator() {
	}

	/**
	 * Add layout manager to the group to be synchronized
	 * 
	 * @param lm
	 *            The layout manager to be added
	 */
	public void addLayoutManager(GELayoutManager lm) {
		if (lm instanceof GEMultiBorderLayout)
			_layoutGroup.add((GEMultiBorderLayout) lm);
	}

	/**
	 * Get the preferred size by getting information from layout managers.
	 */
	public Dimension getPreferredSize(Object object) {
		Dimension preferredSize = new Dimension(0, 0);
		if (DEBUG)
			System.out.println(" " + this + ":" + object);
		if (object instanceof String) {
			String position = (String) object;
			for (GEMultiBorderLayout layout : _layoutGroup) {
				Dimension size = preferredSize;
				size = layout.getPreferredDimensions(position);
				// if (DEBUG) System.out.println("Layout " + layout + ":"
				// +size);
				preferredSize.width = Math.max(size.width, preferredSize.width);
				preferredSize.height = Math.max(size.height,
						preferredSize.height);
			}
		}
		if (DEBUG)
			System.out.println("Preferred Size: " + preferredSize);
		return preferredSize;
	}

	/**
	 * Get the minimum size by getting information from layout managers.
	 */
	public Dimension getMinimumSize(Object object) {
		return getPreferredSize(object);
	}

	/**
   * 
   */
	private ArrayList<GEMultiBorderLayout> _layoutGroup = new ArrayList<GEMultiBorderLayout>();
}

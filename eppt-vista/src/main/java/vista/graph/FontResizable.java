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
 * Any class that implements this interface has a method that allows the
 * resizing of font by scaling, heuristics or strict bounds. Currently only
 * supporting by scaling by ratio.
 *
 * @author Nicky Sandhu
 * @version $Id: FontResizable.java,v 1.1 2003/10/02 20:48:55 redwood Exp $
 */
public interface FontResizable
{
	/**
	 * Sets the font by using the set font size and the ratio.
	 */
	void setFontByRatio(double ratio);
}

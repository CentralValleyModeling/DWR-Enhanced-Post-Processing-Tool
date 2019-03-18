/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

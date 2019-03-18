/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

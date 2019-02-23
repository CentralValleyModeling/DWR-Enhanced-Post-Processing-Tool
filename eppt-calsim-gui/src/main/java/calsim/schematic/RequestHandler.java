/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.schematic;

import java.awt.event.MouseEvent;

/**
 * An interface defining the call back functions when the schematic
 * recives meaninful input
 *
 * @author Nicky Sandhu
 * @version $Id: RequestHandler.java,v 1.1.2.3 1999/05/25 18:03:11 nsandhu Exp $
 */
public interface RequestHandler
{
	/**
	 * called when a symbol with reference object obj is clicked on
	 */
	void clickedOn(Object obj, MouseEvent e);

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	void doubleClickedOn(Object obj, MouseEvent e);

	/**
	 * called when a symbol with reference object obj is moved over
	 */
	void pressedOn(Object obj, MouseEvent e);

	/**
	 * called when a symbol with reference object obj is moved over
	 */
	void releasedOn(Object obj, MouseEvent e);

	/**
	 * called when a symbol with reference object obj is moved over
	 */
	void movedOver(Object obj, MouseEvent e);

	/**
	 * called when a symbol is dragged
	 */
	void draggedTo(Object obj, MouseEvent e);
}

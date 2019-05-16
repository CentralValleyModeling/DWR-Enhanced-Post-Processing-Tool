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

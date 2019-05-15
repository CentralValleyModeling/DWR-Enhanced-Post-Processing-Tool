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

package calsim.schematic.input;

import java.awt.event.MouseEvent;

import calsim.schematic.RequestHandler;

/**
 * It listens for double click. When this handler gets a double click
 * it opens a property dialog for the appropriate type of object.
 * E.g. if a channel object is double clicked then the dialog box with
 * channel property panel will be opened.
 */
public class InputHandler implements RequestHandler
{
	public InputHandler()
	{
	}

	/**
	 * ignore
	 */
	public void clickedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is double clicked on
	 */
	public void doubleClickedOn(Object obj, MouseEvent e)
	{
		// if type == channel show channel properties
		// if type == reservoir show reservoir properties
		// if type == delivery show delivery properties
		// if type == return show return properties
		// if type == inflow show input arc properties
	}

	/**
	 * called when a symbol with reference object obj is pressed on
	 */
	public void pressedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is released on
	 */
	public void releasedOn(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is moved over
	 */
	public void movedOver(Object obj, MouseEvent e)
	{
	}

	/**
	 * called when a symbol with reference object obj is dragged
	 */
	public void draggedTo(Object obj, MouseEvent e)
	{
	}
}

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

import vista.graph.DoublePoint;
import vista.graph.GraphicElement;
//import java.awt.Color;

/**
 * This class contains the symbol to be added, its anchor point and other
 * diagonally opposite points ( in double x,y) and the reference object
 * to be associated with that element.
 *
 * @author Nicky Sandhu
 * @version $Id: CalsimSymbolData.java,v 1.1.2.1 1999/03/08 00:16:20 nsandhu Exp $
 */
public interface CalsimSymbolData
{
	/**
	 * the symbol representing the data
	 */
	GraphicElement getGraphicElement();

	/**
	 * the anchor point or upper left corner of bounds
	 */
	DoublePoint getAnchorPoint();

	/**
	 * the other point or lower left corner of bounds
	 */
	DoublePoint getOtherPoint();

	/**
	 * the reference object to be associated with this element.
	 * This is the data or identifier object.
	 */
	Object getReferenceObject();

	/**
	 * set the reference object for this element.
	 */
	void setReferenceObject(Object obj);
}

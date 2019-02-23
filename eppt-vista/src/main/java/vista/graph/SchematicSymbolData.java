/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;


/**
 * This class contains the symbol to be added, its anchor point and other
 * diagonally opposite points ( in double x,y) and the reference object to be
 * associated with that element.
 *
 * @author Nicky Sandhu
 * @version $Id: SchematicSymbolData.java,v 1.1 1999/12/29 17:06:59 nsandhu Exp
 * $
 */
public interface SchematicSymbolData
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
	 * the reference object to be associated with this element. This is the data
	 * or identifier object.
	 */
	Object getReferenceObject();

	/**
	 * set the reference object for this element.
	 */
	void setReferenceObject(Object obj);
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Color;

/**
 * Data for adding symbol or line to schematic
 *
 * @author Nicky Sandhu
 * @version $Id: SymbolData.java,v 1.3 1998/12/19 01:40:15 nsandhu Exp $
 */
public interface SymbolData
{
	int LINE_SYMBOL = 100;
	int SYMBOL = 101;
	int CIRCLE = 1;
	int TRIANGLE = 1;
	int SQUARE = 1;
	int LABELED_LINE_SYMBOL = 200;
	int LABELED_SYMBOL = 201;

	/**
	 *
	 */
	int getSize();

	/**
	 *
	 */
	Color getColor();

	/**
	 *
	 */
	DoublePoint getAnchorPoint();

	/**
	 *
	 */
	DoublePoint getOtherPoint();

	/**
	 *
	 */
	int getType(); // LINE_SYMBOL or SYMBOL

	/**
	 *
	 */
	int getShape(); // for symbol it's CIRCLE, TRIANGLE, SQUARE

	/**
	 *
	 */
	Object getReferenceObject();

	/**
	 *
	 */
	void setReferenceObject(Object obj);
}

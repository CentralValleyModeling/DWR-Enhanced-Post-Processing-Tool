/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Dimension;

/**
 * This interface is to be implemented by any class that wishes to provide data
 * for the schematic. This interface requires the implementation to iterate over
 * each symbol that needs to be drawn. The data required for drawing a
 * particular symbol is encapsulated in the SymbolData class
 *
 * @author Nicky Sandhu
 * @version $Id: SchematicDataModel.java,v 1.3 1999/12/15 16:29:12 nsandhu Exp $
 * @see SymbolData
 */
public interface SchematicDataModel
{
	/**
	 * resets the data model
	 */
	void reset();

	/**
	 * gets the data required to draw the next symbol.
	 *
	 * @see SymbolData
	 */
	SchematicSymbolData nextSymbolData();

	/**
	 * @return true while has more symbols to be added
	 */
	boolean hasMoreSymbols();

	/**
	 * gets the maximum value for the x axis
	 */
	double getXMax();

	/**
	 * gets the maximum value for the x axis
	 */
	double getXMin();

	/**
	 * gets the maximum value for the x axis
	 */
	double getYMax();

	/**
	 * gets the maximum value for the x axis
	 */
	double getYMin();

	/**
	 * gets the title text for this schematic
	 */
	String getTitleText();

	/**
	 * sets the title text for this schematic
	 */
	void setTitleText(String str);

	/**
	 * gets the width and height of the schematic in screen pixels
	 */
	Dimension getScreenSize();

	/**
	 * sets the width and height of the schematic in screen pixels
	 */
	void setScreenSize(int width, int height);
}

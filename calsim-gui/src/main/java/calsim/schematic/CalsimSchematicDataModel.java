/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package calsim.schematic;

import java.awt.*;

/**
 * This interface is to be implemented by any class that wishes to
 * provide data for the schematic. This interface requires the implementation
 * to iterate over each symbol that needs to be drawn. The data required for
 * drawing a particular symbol is encapsulated in the CalsimSymbolData class
 *
 * @author Nicky Sandhu
 * @version $Id: CalsimSchematicDataModel.java,v 1.1.2.4 1999/07/18 20:57:55 nsandhu Exp $
 * @see CalsimSymbolData
 */
public interface CalsimSchematicDataModel
{
	/**
	 * resets the data model
	 */
	void reset();

	/**
	 * gets the data required to draw the next symbol.
	 *
	 * @see CalsimSymbolData
	 */
	CalsimSymbolData nextSymbolData();

	/**
	 * @return true while has more symbols to be added
	 */
	boolean hasMoreSymbols();
	/**
	 * A rectangle from xmin,ymin to xmax-xmin, ymax-ymin
	 */
	//  public DoubleRect getRealBounds();
	/**
	 * set the bounds from xmin,ymin to xmax,ymax
	 */
	//  public void setRealBounds(double xmin, double ymin, double xmax, double ymax);

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
	//  public void setScreenSize(int width, int height);
}

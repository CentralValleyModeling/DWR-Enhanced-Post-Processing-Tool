/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.graph;

import java.awt.Color;

/**
 * A factory for producing GraphicElement objects.
 */
public interface GraphFactory
{
	/**
	 *
	 */
	Axis createAxis();

	/**
	 *
	 */
	GEContainer createGEContainer();

	/**
	 *
	 */
	Legend createLegend();

	/**
	 *
	 */
	LegendItem createLegendItem();

	/**
	 *
	 */
	TextLine createTextLine();

	/**
	 *
	 */
	TickText createTickText();

	/**
	 *
	 */
	LineElement createLineElement();

	/**
	 *
	 */
	TickLine createTickLine();

	/**
	 *
	 */
	Plot createPlot();

	/**
	 *
	 */
	Curve createCurve();

	/**
	 *
	 */
	Graph createGraph();

	/**
	 *
	 */
	MultiPlot createMultiPlot();

	/**
	 *
	 */
	Color getNextColor();
}

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

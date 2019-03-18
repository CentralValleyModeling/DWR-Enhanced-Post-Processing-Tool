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
public class DefaultGraphFactory implements GraphFactory {
	/**
   *
   */
	public Axis createAxis() {
		return new Axis(new AxisAttr(), AxisAttr.BOTTOM);
	}

	/**
   *
   */
	public GEContainer createGEContainer() {
		return new GEContainer(new GEAttr());
	}

	/**
   *
   */
	public Legend createLegend() {
		LegendAttr legendAttr = new LegendAttr();
		legendAttr._resizeProportionally = true;
		return new Legend(legendAttr);
	}

	/**
   * 
   */
	public LegendItem createLegendItem() {
		return new LegendItem(new LegendItemAttr(), null);
	}

	/**
   *
   */
	public TextLine createTextLine() {
		return new TextLine(new TextLineAttr(), "");
	}

	/**
   *
   */
	public TickText createTickText() {
		return new TickText(new TickTextAttr(), null, null);
	}

	/**
   *
   */
	public LineElement createLineElement() {
		return new LineElement(new LineElementAttr());
	}

	/**
   *
   */
	public TickLine createTickLine() {
		return new TickLine(new TickLineAttr(), null);
	}

	/**
   *
   */
	public Plot createPlot() {
		return new Plot(new PlotAttr());
	}

	/**
   *
   */
	public Curve createCurve() {
		return new DefaultCurve(new CurveAttr(), null);
	}

	/**
	 * creates a default empty graph with default attributes
	 */
	public Graph createGraph() {
		GraphAttr graphAttr = new GraphAttr();
		return new Graph(graphAttr);
	}

	/**
	 * creates an empty multi plot container with default attributes.
	 */
	public MultiPlot createMultiPlot() {
		return new MultiPlot(new PlotAttr());
	}

	/**
   *
   */
	public Color getNextColor() {
		_colorIndex++;
		return _colorTable[_colorIndex % _colorTable.length];
	}

	private int _colorIndex = -1;
	private static Color[] _colorTable = { Color.red, Color.green, Color.blue,
			Color.pink, Color.cyan, Color.orange, Color.magenta,
			new Color(0, 206, 209), // Dark Turquoise
			new Color(85, 107, 47), // Dark Olive Green
			new Color(176, 48, 96), // maroon
			new Color(95, 158, 160), // Cadet Blue
			new Color(218, 112, 214), // orchid
			new Color(160, 32, 240) // purple
	};

}

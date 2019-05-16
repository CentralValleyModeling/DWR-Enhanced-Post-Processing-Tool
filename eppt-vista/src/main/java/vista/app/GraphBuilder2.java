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
package vista.app;

import java.awt.Insets;
import java.util.Date;
import java.util.Enumeration;
import java.util.Vector;

import vista.graph.AxisAttr;
import vista.graph.Curve;
import vista.graph.GEBorderLayout;
import vista.graph.GEContainer;
import vista.graph.Graph;
import vista.graph.GraphAttr;
import vista.graph.Legend;
import vista.graph.LegendAttr;
import vista.graph.LegendItem;
import vista.graph.LegendItemAttr;
import vista.graph.MultiPlot;
import vista.graph.Plot;
import vista.graph.PlotAttr;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;
import vista.set.DataReference;

/**
 * Builds a graph from given data sets.
 *
 * @author Nicky Sandhu
 * @version $Id: GraphBuilder2.java,v 1.1.1.1 1998/09/30 03:57:45 nsandhu Exp $
 */
public class GraphBuilder2 implements GraphBuilder
{
	private int _nrows, _ncols, _curvesPerPlot;
	/**
	 *
	 */
	private Vector _dataRefs;

	/**
	 *
	 */
	public GraphBuilder2()
	{
		_nrows = 2;
		_ncols = 2;
		_curvesPerPlot = 2;
	}

	/**
	 * initializes the graph builder with no data sets
	 */
	public GraphBuilder2(int nrows, int ncols, int curvesPerPlot)
	{
		_dataRefs = new Vector();
		_curvesPerPlot = curvesPerPlot;
		_nrows = nrows;
		_ncols = ncols;
	}

	/**
	 * adds a data set to the graph builder
	 */
	public void addData(DataReference ref)
	{
		if(ref == null)
		{
			return;
		}
		_dataRefs.addElement(ref);
	}

	/**
	 * removes the dat set
	 */
	public void removeData(DataReference ref)
	{
		if(ref == null)
		{
			return;
		}
		_dataRefs.removeElement(ref);
	}

	/**
	 * removes all the data sets
	 */
	public void removeAll()
	{
		if(_dataRefs.size() > 0)
		{
			_dataRefs.removeAllElements();
		}
	}

	/**
	 * creates graph from existing data sets
	 */
	public Graph[] createGraphs()
	{
		if(_dataRefs.size() == 0)
		{
			return null;
		}
		// get the number of graphs and initialize them.
		int ngraphs = (_dataRefs.size() - 1)
				/ (_curvesPerPlot * _nrows * _ncols) + 1;
		Graph[] graphs = new Graph[ngraphs];
		MultiPlot[] plotContainers = new MultiPlot[ngraphs];
		for(int i = 0; i < ngraphs; i++)
		{
			graphs[i] = new Graph(new GraphAttr());
			plotContainers[i] = new MultiPlot(new PlotAttr());
			// plotContainers[i].setLayout( new GEGridLayout(_nrows, _ncols) );
			graphs[i].addPlot(plotContainers[i]);
		}
		// create and add curves to plots to the graphs
		int gindex = 0;
		Graph graph = null;
		GEContainer pc = null;
		Plot plot = null;
		Legend lg = null;
		int numberAdded = 0;
		for(Enumeration e = _dataRefs.elements(); e.hasMoreElements(); )
		{
			DataReference ref = (DataReference) e.nextElement();
			graph = graphs[gindex];
			pc = plotContainers[gindex];
			graph.setTitle(GraphTemplate.generateTitleText(_dataRefs));
			graph.setInsets(new Insets(10, 25, 10, 25));
			Curve c = CurveFactory.createCurve(ref, AxisAttr.BOTTOM,
					AxisAttr.LEFT, GraphTemplate.generateLegendText(ref));
			if(plot == null || plot.getNumberOfCurves() > _curvesPerPlot)
			{
				if(plot != null)
				{
					plot.addLegend(lg, AxisAttr.LEFT);
				}
				plot = new Plot(new PlotAttr());
				lg = new Legend(new LegendAttr());
				if(numberAdded == _nrows * _ncols)
				{
					gindex++;
					if(graph != null)
					{
						// add date and time.
						TextLineAttr dateAttr = new TextLineAttr();
						dateAttr._font = new java.awt.Font("Times Roman",
								java.awt.Font.PLAIN, 8);
						dateAttr._foregroundColor = java.awt.Color.red;
						dateAttr._resizeProportionally = true;
						dateAttr._justification = TextLineAttr.RIGHT;
						graph.add(GEBorderLayout.SOUTH, new TextLine(dateAttr,
								new Date().toString()));
					}
					graph = graphs[gindex];
					pc = plotContainers[gindex];
					numberAdded = 0;
				}
				pc.add(plot);
				numberAdded++;
			}
			else
			{
			}
			plot.addCurve(c);
			lg.add(new LegendItem(new LegendItemAttr(), c));
		}
		if(plot != null)
		{
			// add date and time only if missed adding in above loop
			if(graph != null)
			{
				TextLineAttr dateAttr = new TextLineAttr();
				dateAttr._font = new java.awt.Font("Times Roman",
						java.awt.Font.PLAIN, 8);
				dateAttr._foregroundColor = java.awt.Color.red;
				dateAttr._resizeProportionally = true;
				dateAttr._justification = TextLineAttr.RIGHT;
				graph.add(GEBorderLayout.SOUTH, new TextLine(dateAttr,
						new Date().toString()));
			}
			plot.addLegend(lg, AxisAttr.LEFT);
		}
		return graphs;
	}
}

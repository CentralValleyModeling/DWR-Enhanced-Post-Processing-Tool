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

import java.awt.Color;
import java.util.ArrayList;
import java.util.Date;

import vista.graph.Axis;
import vista.graph.Curve;
import vista.graph.CurveAttr;
import vista.graph.DefaultGraphFactory;
import vista.graph.GEContainer;
import vista.graph.Graph;
import vista.graph.GraphFactory;
import vista.graph.GraphicElement;
import vista.graph.Legend;
import vista.graph.LegendItem;
import vista.graph.LegendItemAttr;
import vista.graph.MultiPlot;
import vista.graph.Plot;
import vista.graph.SimpleTickGenerator;
import vista.graph.TextLine;
import vista.graph.TextLineAttr;
import vista.graph.TickGenerator;
import vista.graph.TimeTickGenerator;
import vista.set.DataReference;
import vista.set.DataRetrievalException;
import vista.set.DataSet;

/**
 * Builds a graph from given data sets.
 *
 * @author Nicky Sandhu
 * @version $Id: DefaultGraphBuilder.java,v 1.1 2003/10/02 20:48:28 redwood Exp
 * $
 */
public class DefaultGraphBuilder implements GraphBuilder
{
	/**
	 * debuggin' flag...
	 */
	private static final boolean DEBUG = false;
	/**
	 *
	 */
	private ArrayList<DataReference> _dataRefs;
	/**
	 *
	 */
	private GraphFactory _factory = new DefaultGraphFactory();

	/**
	 * initializes the graph builder with no data sets
	 */
	public DefaultGraphBuilder()
	{
		_dataRefs = new ArrayList<DataReference>();
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
		_dataRefs.add(ref);
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
		_dataRefs.remove(ref);
	}

	/**
	 * removes all the data sets
	 */
	public void removeAll()
	{
		if(_dataRefs.size() > 0)
		{
			_dataRefs.clear();
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
		DataReference[] drefs = new DataReference[_dataRefs.size()];
		drefs = _dataRefs.toArray(drefs);
		GraphBuilderInfo info = new GraphBuilderInfo(drefs, MainProperties
				.getProperties());
		if(DEBUG)
		{
			System.out.println(info);
		}
		Graph[] graphs = new Graph[info.getNumberOfGraphs()];
		for(int i = 0; i < info.getNumberOfGraphs(); i++)
		{
			graphs[i] = createGraphForIndex(info, i);
		}
		return graphs;
	}

	/**
	 * creates graph from data sets
	 */
	private Graph createGraphForIndex(GraphBuilderInfo info, int index)
	{
		String title = info.getGraphTitle(index);

		// create container hierarchy of graph -> multi plot -> plot
		Graph graph = _factory.createGraph();
		MultiPlot multiPlot = _factory.createMultiPlot();
		graph.addPlot(multiPlot);
		//
		int nplots = info.getNumberOfPlots(index);
		for(int i = 0; i < nplots; i++)
		{
			multiPlot.add(_factory.createPlot());
		}

		// set title
		graph.setTitle(info.getGraphTitle(index));
		// Graph Insets: Template file
		graph.setInsets(new java.awt.Insets(5, 20, 10, 25));
		// Legend attributes: Template file
		Legend legend = _factory.createLegend();
		graph.setLegend(legend);

		for(int pIndex = 0; pIndex < nplots; pIndex++)
		{

			multiPlot.setCurrentPlot(pIndex);
			// Plot Attributes: Template file
			Plot plot = graph.getPlot();
			plot.setInsets(new java.awt.Insets(20, 5, 20, 50));
			// add axes to plot
			int numberOfAxes = 4;
			TickGenerator simpleTG = new SimpleTickGenerator();
			TickGenerator timeTG = new TimeTickGenerator();
			for(int i = 1; i <= numberOfAxes; i++)
			{
				String axisLabel = info.getAxisLabel(index, pIndex, i);
				if(axisLabel != null)
				{
					Axis axis = plot.createAxis(i);
					axis.setAxisLabel(axisLabel);
					if(axisLabel.equals("TIME"))
					{
						axis.setTickGenerator(timeTG);
					}
					else
					{
						axis.setTickGenerator(simpleTG);
					}
				}
			}
			// add data to plot
			DataReference[] refs = info.getDataReferences(index, pIndex);
			if(refs == null)
			{
				throw new IllegalArgumentException(
						"All references to be graphed are null");
			}
			for(int i = 0; i < refs.length; i++)
			{
				if(DEBUG)
				{
					System.out.println("Adding reference " + refs[i]
							+ " to plot");
				}
				int xPos = info.getXAxisPosition(refs[i]);
				int yPos = info.getYAxisPosition(refs[i]);
				try
				{
					DataSet ds = refs[i].getData();
					Curve crv = CurveFactory.createCurve(refs[i], xPos, yPos,
							info.getLegendLabel(refs[i]));
					plot.addCurve(crv);
					CurveAttr ca = (CurveAttr) crv.getAttributes();
					Color currentColor = _factory.getNextColor();
					ca._foregroundColor = currentColor;
					if(DEBUG)
					{
						System.out.println("Done adding curve");
					}
					LegendItem li = _factory.createLegendItem();
					li.setLegendName(info.getLegendLabel(refs[i]));
					li.setCurve(crv);
					LegendItemAttr lia = (LegendItemAttr) li.getAttributes();
					legend.add(li);
					lia._foregroundColor = Color.black;
				}
				catch(DataRetrievalException dre)
				{
					dre.printStackTrace();
				}
			}
		}

		// Scale components in layout resizing: Template file
		// GEBorderLayout plotLayout = (GEBorderLayout) plot.getLayout();
		// plotLayout.setScaleComponents(true);
		TextLineAttr dateAttr = new TextLineAttr();
		dateAttr._font = new java.awt.Font("Times Roman", java.awt.Font.PLAIN,
				10);
		dateAttr._foregroundColor = java.awt.Color.red;
		dateAttr._resizeProportionally = true;
		dateAttr._justification = TextLineAttr.RIGHT;
		graph.getLegend().add(new TextLine(dateAttr, new Date().toString()));

		if(DEBUG)
		{
			printAll(graph);
		}
		return graph;
	}

	/**
	 *
	 */
	void printAll(GEContainer c)
	{
		System.out.println(c + " { ");
		int count = c.getElementCount();
		for(int i = 0; i < count; i++)
		{
			GraphicElement ge = c.getElement(i);
			if(ge instanceof GEContainer)
			{
				printAll((GEContainer) ge);
			}
			else
			{
				print(ge);
			}
		}
		System.out.println(" }");
	}

	/**
	 *
	 */
	void print(GraphicElement ge)
	{
		System.out.println(ge);
	}
}

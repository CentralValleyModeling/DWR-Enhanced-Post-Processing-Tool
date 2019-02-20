/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.util.ArrayList;

import vista.set.DataReference;

/**
 * @author Nicky Sandhu
 * @version $Id: GraphInfo.java,v 1.1 2003/10/02 20:48:30 redwood Exp $
 */
class GraphInfo
{
	private int PLOTS_PER_GRAPH = 2;
	private int index;
	private String title;
	private ArrayList<PlotInfo> plots;

	/**
	 *
	 */
	public GraphInfo(int index, String title)
	{
		plots = new ArrayList<PlotInfo>();
	}

	/**
	 *
	 */
	public int getNumberOfPlots()
	{
		return plots.size();
	}

	/**
	 *
	 */
	public PlotInfo getPlotInfo(int index)
	{
		return plots.get(index);
	}

	/**
	 *
	 */
	public DataSetInfo addDataReference(DataReference ref)
			throws GraphLimitException
	{
		DataSetInfo dsi = null;

		boolean setAdded = false;

		for(PlotInfo pInfo : plots)
		{
			try
			{
				dsi = pInfo.addDataReference(ref);
				setAdded = true;
				break;
			}
			catch(PlotLimitException ple)
			{
			}
		}
		if(!setAdded)
		{
			if(limitReached())
			{
				throw new GraphLimitException();
			}
			PlotInfo pInfo = new PlotInfo();
			plots.add(pInfo = new PlotInfo());
			pInfo.setIndex(plots.size());
			pInfo.setTitle("Plot # " + plots.size());
			pInfo.setGraphIndex(index);
			try
			{
				dsi = pInfo.addDataReference(ref);
			}
			catch(PlotLimitException ple)
			{
				throw new GraphLimitException();
			}
		}
		return dsi;
	}

	/**
	 *
	 */
	public boolean limitReached()
	{
		return (plots.size() >= PLOTS_PER_GRAPH);
	}

	/**
	 *
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer(25);
		buf.append("Graph # ").append(index).append("\n");
		buf.append("Graph title ").append(index).append("\n");
		buf.append("# of plots ").append(plots.size()).append("\n");
		for(int i = 0; i < plots.size(); i++)
		{
			buf.append(plots.get(i).toString());
		}
		return buf.toString();
	}
}

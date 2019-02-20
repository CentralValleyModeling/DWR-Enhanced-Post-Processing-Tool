/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.util.ArrayList;
import java.util.Properties;

import vista.graph.AxisAttr;
import vista.set.DataReference;
import vista.set.PathnameFormat;

/**
 * This class calculates the x and y axis positions of the given data sets. Each
 * unique pair of units is assigned a pair of axes. Each data set is assigned a
 * pair of units in the existing pair of units or a pair of such units is
 * created for it.
 */
public class GraphBuilderInfo
{
	static int DATA_PER_PLOT = 5;
	static int PLOTS_PER_GRAPH = 2;
	static boolean DEBUG = false;
	private static int ngraphs = 1;
	private ArrayList<DataReference> _dataRefs;
	private ArrayList<GraphInfo> _graphInfo;
	private ArrayList<DataSetInfo> _dataSetInfo;
	private Properties _props;
	private boolean _useUnits;

	/**
	 * create an empty graph info object
	 */
	public GraphBuilderInfo(Properties props)
	{
		_props = props;
		_useUnits = _props.getProperty("graph.useUnits").indexOf("true") >= 0;
		_dataRefs = new ArrayList<DataReference>();
		_graphInfo = new ArrayList<GraphInfo>();
		_dataSetInfo = new ArrayList<DataSetInfo>();
	}

	/**
	 * Constructor. Builds the graph information from the given data set
	 * applying simple rules.
	 */
	public GraphBuilderInfo(DataReference[] dsArray, Properties props)
	{
		this(props);
		if(dsArray == null)
		{
			return;
		}
		for(int i = 0; i < dsArray.length; i++)
		{
			if(dsArray[i] != null)
			{
				_dataRefs.add(dsArray[i]);
			}
		}
		if(_dataRefs.size() == 0)
		{
			throw new IllegalArgumentException("All data sets are null");
		}
		updateInfo();
	}

	/**
	 *
	 */
	public static String getTitleTemplate()
	{
		return MainProperties.getProperty("graph.titleTemplate");
	}

	/**
	 *
	 */
	public static String getLegendTemplate()
	{
		return MainProperties.getProperty("graph.legendTemplate");
	}

	/**
	 * add data set to graph info.
	 */
	public void add(DataReference ds)
	{
		if(ds == null)
		{
			return;
		}
		_dataRefs.add(ds);
		updateInfo();
	}

	/**
	 * remove data set and regenerate graphs and plots information
	 */
	public void remove(DataReference ds)
	{
		if(ds == null)
		{
			return;
		}
		_dataRefs.remove(ds);
		updateInfo();
	}

	/**
	 * Gets the number of graphs
	 */
	public int getNumberOfGraphs()
	{
		return _graphInfo.size();
	}

	/**
	 * get number of plots for given graph index
	 */
	public int getNumberOfPlots(int graphIndex)
	{
		if(graphIndex >= getNumberOfGraphs())
		{
			return 0;
		}
		else
		{
			return _graphInfo.get(graphIndex).getNumberOfPlots();
		}
	}

	/**
	 * gets the graph title for graph
	 */
	public String getGraphTitle(int graphIndex)
	{
		if(graphIndex >= getNumberOfGraphs())
		{
			return "";
		}
		// else return ((GraphInfo) _graphInfo.at(graphIndex)).title;
		else
		{
			DataReference[] refs = new DataReference[_dataRefs.size()];
			refs = _dataRefs.toArray(refs);
			return PathnameFormat.format(getTitleTemplate(), refs);
		}
	}

	/**
	 * returns the legend label for the given data set
	 */
	public String getLegendLabel(DataReference ds)
	{
		DataSetInfo dsi = getDataSetInfo(ds);
		if(dsi == null)
		{
			return null;
		}
		else
		{
			return dsi.getLegendLabel();
		}
	}

	/**
	 * Gets the graph index for the particular data set only if the data set had
	 * been in the data set array specified in the constructor.
	 *
	 * @return the index of graph to which data set belongs. if data sets is not
	 * found in that array, -1 is returned
	 */
	public int getGraphIndex(DataReference ds)
	{
		DataSetInfo dsi = getDataSetInfo(ds);
		if(dsi == null)
		{
			return -1;
		}
		else
		{
			return dsi.getGraphIndex();
		}
	}

	/**
	 * return index of plot within graph in which data set is viewed
	 */
	public int getPlotIndex(DataReference ds)
	{
		DataSetInfo dsi = getDataSetInfo(ds);
		if(dsi == null)
		{
			return -1;
		}
		else
		{
			return dsi.getPlotIndex();
		}
	}

	/**
	 * returns the x axis position of the data set.
	 */
	public int getXAxisPosition(DataReference ds)
	{
		DataSetInfo dsi = getDataSetInfo(ds);
		if(dsi == null)
		{
			return -1;
		}
		else
		{
			return dsi.getXAxisPosition();
		}
	}

	/**
	 * returns the y axis position of the data set.
	 */
	public int getYAxisPosition(DataReference ds)
	{
		DataSetInfo dsi = getDataSetInfo(ds);
		if(dsi == null)
		{
			return -1;
		}
		else
		{
			return dsi.getYAxisPosition();
		}
	}

	/**
	 * @param axisPosition an integer specifying the axis location.
	 * @return the axis label for given graph index and axis position
	 * @see AxisAttr
	 */
	public String getAxisLabel(int graphIndex, int plotIndex, int axisPosition)
	{
		PlotInfo pInfo = getPlotInfo(graphIndex, plotIndex);
		return pInfo.getAxisLabel(axisPosition);
	}

	/**
	 * gets an array of data sets which belong to a certain graph index.
	 */
	public DataReference[] getDataReferences(int graphIndex, int plotIndex)
	{
		PlotInfo pInfo = getPlotInfo(graphIndex, plotIndex);
		if(pInfo == null)
		{
			return null;
		}
		return pInfo.getDataReferences();
	}

	/**
	 * update information as data sets have been added or removed
	 */
	private void updateInfo()
	{
		if(_graphInfo.size() > 0)
		{
			for(int i = ngraphs - 1; i >= 0; i--)
			{
				_graphInfo.remove(i);
			}
			ngraphs = 1;
		}
		for(DataReference r : _dataRefs)
		{
			_dataSetInfo.add(addSetInfo(r));
		}
	}

	/**
	 *
	 */
	private DataSetInfo addSetInfo(DataReference ref)
	{
		DataSetInfo dsi = null;
		if(_graphInfo.size() == 0)
		{
			GraphInfo info = new GraphInfo(ngraphs, "Graph # " + ngraphs);
			_graphInfo.add(info);
		}
		while(true)
		{
			try
			{
				GraphInfo info = _graphInfo.get(_graphInfo.size() - 1);
				dsi = info.addDataReference(ref);
				break;
			}
			catch(GraphLimitException e)
			{
				int index = ngraphs++;
				GraphInfo newInfo = new GraphInfo(index, "Graph # " + index);
				_graphInfo.add(newInfo);
			}
		}
		return dsi;
	}

	/**
	 *
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer(getClass().getName());
		buf.append("\n").append("Number Of Graphs: ").append(
				getNumberOfGraphs()).append("\n");
		for(int i = 0; i < getNumberOfGraphs(); i++)
		{
			buf.append(_graphInfo.get(i).toString());
		}
		buf.append("---------").append("\n");
		return buf.toString();
	}

	/**
	 *
	 */
	public GraphInfo getGraphInfo(int graphIndex)
	{
		return _graphInfo.get(graphIndex);
	}

	/**
	 *
	 */
	public PlotInfo getPlotInfo(int graphIndex, int plotIndex)
	{
		GraphInfo gInfo = getGraphInfo(graphIndex);
		return gInfo.getPlotInfo(plotIndex);
	}

	/**
	 *
	 */
	public DataSetInfo getDataSetInfo(DataReference ref)
	{
		for(DataSetInfo dsi : _dataSetInfo)
		{
			if(ref.equals(dsi.getReference()))
			{
				return dsi;
			}
		}
		return null;
	}
}// end of class GraphBuilder Info

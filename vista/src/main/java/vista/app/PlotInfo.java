/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.util.ArrayList;

import vista.graph.AxisAttr;
import vista.set.DataReference;
import vista.set.DataRetrievalException;
import vista.set.DataSet;
import vista.set.DataSetAttr;
import vista.set.PathnameFormat;

/**
 * information for each plot.
 *
 * @author Nicky Sandhu
 * @version $Id: PlotInfo.java,v 1.1 2003/10/02 20:48:38 redwood Exp $
 */
class PlotInfo
{
	private int DATA_PER_PLOT = 5;
	private int _graphIndex;
	private int _index;
	private String _title;
	private String[] _axisLabels;
	private ArrayList<DataReference> _dataRefs;
	private ArrayList<DataSetInfo> _dataSetInfos;
	private int _maximumDataLimit;

	/**
	 * initializes plot information class
	 */
	public PlotInfo()
	{
		_dataRefs = new ArrayList<DataReference>();
		_dataSetInfos = new ArrayList<DataSetInfo>();
		_axisLabels = new String[5];
		_maximumDataLimit = 5;
	}

	/**
	 *
	 */
	public void setMaximumDataLimit(int n)
	{
		_maximumDataLimit = n;
	}

	void setAxisLabel(int pos, String str)
	{
		_axisLabels[pos] = str;
	}

	/**
	 *
	 */
	public int getGraphIndex()
	{
		return _graphIndex;
	}

	void setGraphIndex(int i)
	{
		_graphIndex = i;
	}

	/**
	 *
	 */
	public int getIndex()
	{
		return _index;
	}

	void setIndex(int i)
	{
		_index = i;
	}

	/**
	 *
	 */
	public String getTitle()
	{
		return _title;
	}

	void setTitle(String str)
	{
		_title = str;
	}

	/**
	 *
	 */
	public String getAxisLabel(int axisPosition)
	{
		// pre-condition: is axis position valid?
		if(axisPosition == AxisAttr.BOTTOM || axisPosition == AxisAttr.TOP
				|| axisPosition == AxisAttr.RIGHT
				|| axisPosition == AxisAttr.LEFT)
		{
			return _axisLabels[axisPosition];
		}
		else
		{
			return null;
		}
	}

	/**
	 *
	 */
	public DataReference[] getDataReferences()
	{
		// pre-condition: atleast one data set is available
		int nd = _dataRefs.size();
		if(nd == 0)
		{
			return null;
		}
		DataReference[] ds = new DataReference[nd];
		return _dataRefs.toArray(ds);
	}

	/**
	 * adds the given reference to the plot if possible
	 *
	 * @return the data set information for the given reference
	 * @throw PlotLimitException
	 */
	public DataSetInfo addDataReference(DataReference ref)
			throws PlotLimitException
	{
		// pre-conditions ref != null and not already in the plot.
		if(ref == null)
		{
			return null;
		}
		DataSet ds = null;
		try
		{
			ds = ref.getData();
		}
		catch(Exception e)
		{
			return null;
		}
		if(!hasSpaceFor(ref) || limitReached())
		{
			throw new PlotLimitException();
		}
		if(!_dataRefs.contains(ref))
		{
			_dataRefs.add(ref);
		}
		//
		DataSetAttr attr = ds.getAttributes();
		String xUnits = attr.getXUnits().trim().toUpperCase();
		String yUnits = attr.getYUnits().trim().toUpperCase();
		DataSetInfo dsi = new DataSetInfo(ref);
		dsi.setGraphIndex(getGraphIndex());
		dsi.setPlotIndex(getIndex());
		if(_axisLabels[AxisAttr.BOTTOM] == null)
		{
			_axisLabels[AxisAttr.BOTTOM] = xUnits;
			dsi.setXAxisPosition(AxisAttr.BOTTOM);
		}
		else if(_axisLabels[AxisAttr.BOTTOM].equals(xUnits))
		{
			dsi.setXAxisPosition(AxisAttr.BOTTOM);
		}
		else if(_axisLabels[AxisAttr.TOP] == null)
		{
			_axisLabels[AxisAttr.TOP] = xUnits;
			dsi.setXAxisPosition(AxisAttr.TOP);
		}
		else if(_axisLabels[AxisAttr.TOP].equals(xUnits))
		{
			dsi.setXAxisPosition(AxisAttr.TOP);
		}
		//
		if(_axisLabels[AxisAttr.LEFT] == null)
		{
			_axisLabels[AxisAttr.LEFT] = yUnits;
			dsi.setYAxisPosition(AxisAttr.LEFT);
		}
		else if(_axisLabels[AxisAttr.LEFT].equals(yUnits))
		{
			dsi.setYAxisPosition(AxisAttr.LEFT);
		}
		else if(_axisLabels[AxisAttr.RIGHT] == null)
		{
			_axisLabels[AxisAttr.RIGHT] = yUnits;
			dsi.setYAxisPosition(AxisAttr.RIGHT);
		}
		else if(_axisLabels[AxisAttr.RIGHT].equals(yUnits))
		{
			dsi.setYAxisPosition(AxisAttr.RIGHT);
		}
		dsi.setLegendLabel(PathnameFormat.format(GraphBuilderInfo
				.getLegendTemplate(), ref));
		_dataSetInfos.add(dsi);
		return dsi;
	}

	/**
	 * checks to see if plot has space for more axes
	 */
	public boolean hasSpaceFor(DataReference ref)
	{
		DataSet ds = null;
		try
		{
			ds = ref.getData();
		}
		catch(DataRetrievalException dre)
		{
			return false;
		}
		DataSetAttr dsa = ds.getAttributes();
		String xUnits = dsa.getXUnits().trim().toUpperCase();
		String yUnits = dsa.getYUnits().trim().toUpperCase();
		boolean xAxisHasSpace = (xUnits.equals(_axisLabels[AxisAttr.BOTTOM])
				|| xUnits.equals(_axisLabels[AxisAttr.TOP])
				|| _axisLabels[AxisAttr.BOTTOM] == null || _axisLabels[AxisAttr.TOP] == null);
		boolean yAxisHasSpace = (yUnits.equals(_axisLabels[AxisAttr.LEFT])
				|| yUnits.equals(_axisLabels[AxisAttr.RIGHT])
				|| _axisLabels[AxisAttr.LEFT] == null || _axisLabels[AxisAttr.RIGHT] == null);
		return xAxisHasSpace && yAxisHasSpace;
	}

	/**
	 * checks to see if limit has been reached for the number of data sets
	 * allowed on each plot
	 */
	public boolean limitReached()
	{
		return (_dataRefs.size() >= DATA_PER_PLOT);
	}

	/**
	 * a string representation of this plot
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer(300);
		String eol = System.getProperty("line.separator");
		buf.append("Plot # ").append(_index).append(eol);
		buf.append("Plot title ").append(_title).append(eol);
		buf.append("Bottom axis label ").append(_axisLabels[AxisAttr.BOTTOM])
		   .append(eol);
		buf.append("Top axis label ").append(_axisLabels[AxisAttr.TOP]).append(
				eol);
		buf.append("Left axis label ").append(_axisLabels[AxisAttr.LEFT])
		   .append(eol);
		buf.append("Right axis label ").append(_axisLabels[AxisAttr.RIGHT])
		   .append(eol);
		buf.append("# of data sets ").append(_dataRefs.size()).append(eol);
		return buf.toString();
	}
}

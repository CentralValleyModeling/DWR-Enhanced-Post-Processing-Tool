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

/**
 * This class encapsulates tick data information such as the data values at
 * which to place ticks and the length of the tick as a percentage of the
 * drawing area.
 *
 * @author Nicky Sandhu (DWR).
 * @version $Id: TickData.java,v 1.1 2003/10/02 20:49:10 redwood Exp $
 */
public class TickData
{
	/**
	 * for debuggin
	 */
	public static final boolean DEBUG = false;
	private double _dataMin, _dataMax;
	/**
	 * scale factor = screenLength/dataRange
	 */
	private Scale _scale;
	/**
	 * number of tick marks
	 */
	private int _number;
	/**
	 * an array of data values spanning the entire range. These data values
	 * correspond to tick positions.
	 */
	private double[] _values;
	/**
	 * an array of labels for each index of the _values array.
	 */
	private String[] _labels;

	/**
	 * constructor
	 *
	 * @param values            An array containing the data values at which to place the tick
	 *                          marks at.
	 * @param percentTickLength The length of the ticks as a percentage of the drawing area.
	 */
	public TickData(double[] values, String[] labels, double dataMin,
					double dataMax)
	{
		_values = values;
		_number = _values.length;
		_labels = labels;
		_dataMin = dataMin;
		_dataMax = dataMax;
		_scale = new Scale(_dataMin, _dataMax, 0, 10);
	}

	/**
	 * returns the maximum data coordinate value
	 */
	public double getMaxDCValue()
	{
		return _scale.getDataMaximum();
	}

	/**
	 * returns the minimum data coordinate value
	 */
	public double getMinDCValue()
	{
		return _scale.getDataMinimum();
	}

	/**
	 * sets scale to new data range
	 */
	public void setDCRange(double min, double max)
	{
		_scale.setDCRange(min, max);
	}

	/**
	 * sets scale to new user coordinate range.
	 */
	public void setUCRange(int amin, int amax)
	{
		_scale.setUCRange(amin, amax);
	}

	/**
	 * returns user coordinate value of the indexed tick
	 *
	 * @param index The tick at index in the tick values array
	 * @return The tick value converted to user coordinates
	 */
	public int getUCValue(int index)
	{
		if(DEBUG && index == 0)
		{
			System.out.println(_values[index]);
		}
		if(DEBUG && index == 0)
		{
			System.out.println(_scale.scaleToUC(_values[index]));
		}
		return _scale.scaleToUC(_values[index]);
	}

	/**
	 * Returns the data coordinate value for the indexed tick
	 *
	 * @param index The tick at index in the tick values array
	 * @return The tick value
	 */
	public double getDCValue(int index)
	{
		return _values[index];
	}

	/**
	 * gets scale of tick data
	 */
	public Scale getScale()
	{
		return _scale;
	}

	/**
	 * get number of tick marks in complete range.
	 */
	public int getNumber()
	{
		return _number;
	}

	/**
	 * get label for each tick mark.
	 */
	public String[] getLabels()
	{
		return _labels;
	}

	/**
	 *
	 */
	public String toString()
	{
		StringBuffer sb = new StringBuffer(this.getClass().getName());
		sb.append("Number of ticks = " + this.getNumber() + "\n");
		sb.append("TickValues: Index, Value \n");
		for(int i = 0; i < this.getNumber(); i++)
		{
			sb.append(i + ", " + this.getDCValue(i) + " | ");
		}
		sb.append("\n");
		return sb.toString();
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import java.text.NumberFormat;
import javax.swing.*;
import javax.swing.table.AbstractTableModel;

import vista.set.DataSetElement;
import vista.set.DataSetIterator;
import vista.set.MultiIterator;
import vista.set.TimeSeries;
import vista.time.Time;
import vista.time.TimeFactory;

/**
 * An adapter to adapt the data sets to the table model to allow representation
 * of the data set as a table.
 *
 * @author Nicky Sandhu
 * @version $Id: MultiDataSetTableModel.java,v 1.1 2003/10/02 20:48:35 redwood
 * Exp $
 */
public class MultiDataSetTableModel extends AbstractTableModel
{
	/**
	 * data set
	 */
	private TimeSeries[] _ts;
	/**
	 * iterator
	 */
	private DataSetIterator _dsi;
	/**
	 * row count and column count
	 */
	private int _dataCount, _numberOfColumns;
	/**
	 * The header for x values
	 */
	private String _xColumnHeader;
	private String[] _columnNames;
	private boolean _flagDisplayed;
	/**
	 * time format
	 */
	private Time _time;
	/**
	 * number format
	 */
	private NumberFormat _formatter = NumberFormat.getInstance();
	/**
	 * number format
	 */
	private NumberFormat _flagFormatter = NumberFormat.getInstance();
	/**
	 *
	 */
	private TimeFactory _tf = TimeFactory.getInstance();

	/**
	 * constructs an adapter for data set for use by JTable
	 */
	public MultiDataSetTableModel(TimeSeries[] ts)
	{
		_columnNames = new String[ts.length];
		_xColumnHeader = "TIME";
		_dsi = new MultiIterator(ts);
		_dataCount = 0;
		while(!_dsi.atEnd())
		{
			_dataCount++;
			_dsi.advance();
		}
		// System.out.println("Data count = " + _dataCount);
		_dsi.resetIterator();
		for(int i = 0; i < ts.length; i++)
		{
			_columnNames[i] = ts[i].getName();
		}
		_numberOfColumns = _dsi.getElement().getDimension();
		_flagDisplayed = false;
	}

	/**
	 * returns the number of rows or size of data...
	 */
	public int getRowCount()
	{
		return _dataCount;
	}

	/**
	 * The number of columns 2 or 3 depending upon whether or not to show flags
	 */
	public int getColumnCount()
	{
		if(_flagDisplayed)
		{
			return _numberOfColumns * 2 - 1;
		}
		else
		{
			return _numberOfColumns;
		}
	}

	/**
	 * true if flags are displayed in table
	 */
	public boolean isFlagDisplayed()
	{
		return _flagDisplayed;
	}

	/**
	 * false if flags are displayed in table.
	 */
	public void setFlagDisplayed(boolean b)
	{
		_flagDisplayed = b;
	}

	/**
	 * true except for time string in time series
	 */
	public boolean isCellEditable(int rowIndex, int columnIndex)
	{
		return columnIndex != 0;
	}

	/**
	 * returns name of column for given index
	 */
	public String getColumnName(int columnIndex)
	{
		if(columnIndex == 0)
		{
			return _xColumnHeader;
		}
		else
		{
			if(_flagDisplayed)
			{
				if(columnIndex % 2 == 1)
				{
					return _columnNames[(columnIndex + 1) / 2 - 1];
				}
				else
				{
					return "FLAG VALUE";
				}
			}
			else
			{
				return _columnNames[columnIndex - 1];
			}
		}
	}

	/**
	 * returns value of object at row, column.
	 */
	public Object getValueAt(int rowIndex, int columnIndex)
	{
		_dsi.positionAtIndex(rowIndex);
		if(_flagDisplayed)
		{
			if(columnIndex == 0)
			{
				return _dsi.getElement().getXString(columnIndex);
			}
			else if(columnIndex % 2 == 1)
			{
				return _dsi.getElement().getXString((columnIndex + 1) / 2);
			}
			else
			{
				return _dsi.getElement().getFlagString(columnIndex / 2 - 1);
			}
		}
		else
		{
			return _dsi.getElement().getXString(columnIndex);
		}
	}

	/**
	 * sets value to edited value...
	 */
	public void setValueAt(Object aValue, int rowIndex, int columnIndex)
	{
		_dsi.positionAtIndex(rowIndex);
		if(columnIndex > 0)
		{
			DataSetElement dse = _dsi.getElement();
			try
			{
				if(_flagDisplayed)
				{
					if(columnIndex == 0)
					{
						dse.setX(columnIndex, _formatter.parse((String) aValue)
														.doubleValue());
					}
					else if(columnIndex % 2 == 1)
					{
						dse.setX((columnIndex + 1) / 2, _formatter.parse(
								(String) aValue).doubleValue());
					}
					else
					{
						// don't allow editing of flags...
						// dse.setFlag(columnIndex/2-1,_formatter.parse(
						// (String) aValue).intValue());
					}
				}
				else
				{
					dse.setX(columnIndex, _formatter.parse((String) aValue)
													.doubleValue());
				}
			}
			catch(java.text.ParseException pe)
			{
				JOptionPane.showMessageDialog(null, pe.getMessage());
			}
			_dsi.putElement(dse);
		}
	}
}

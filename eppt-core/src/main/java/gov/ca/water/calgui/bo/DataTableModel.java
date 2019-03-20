/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
//! Root table model for tabular data in the GUI

import javax.swing.table.AbstractTableModel;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IAuditSvc;
import gov.ca.water.calgui.techservice.impl.AuditSvcImpl;

/**
 * This class is used to hold the tables for the application.
 *
 * @author Mohan
 */
public class DataTableModel extends AbstractTableModel
{
	/**
	 * see {@link AuditSvcImpl}
	 */
	private IAuditSvc _auditSvc = AuditSvcImpl.getAuditSvcImplInstance();
	/**
	 * This will hold the value of the table name in this table.
	 */
	private String _tableName = "";
	/**
	 * This will hold all the column names in this table.
	 */
	private String[] _columnNames;
	/**
	 * This will hold all the data values that will be displayed in the table.
	 */
	private Object[][] _data;
	/**
	 * This will tell whether the table can be modify or not.
	 */
	private boolean _isCellEditable;
	/**
	 * The object of the GUI.
	 */
	public DataTableModel(String tableName, String[] columnName, Object[][] data, boolean isCellEditable)
	{

		_tableName = tableName;
		_columnNames = columnName;
		_data = data;
		_isCellEditable = isCellEditable;
	}

	@Override
	public Object clone()
	{
		String tableName = this._tableName;
		String[] colNames = new String[this._columnNames.length];
		System.arraycopy(this._columnNames, 0, colNames, 0, colNames.length);
		Object[][] data1 = new Object[this._data.length][this._data[0].length];
		for(int i = 0; i < data1.length; i++)
		{
			System.arraycopy(this._data[i], 0, data1[i], 0, data1[0].length);
		}
		return new DataTableModel(tableName, colNames, data1, _isCellEditable);
	}

	public String[] getColumnNames()
	{
		return _columnNames;
	}

	public void setColumnNames(String[] columnNames)
	{
		this._columnNames = columnNames;
	}

	public Object[][] getData()
	{
		return _data;
	}

	public void setData(Object[][] data)
	{
		this._data = data;
	}

	public boolean isCellEditable()
	{
		return _isCellEditable;
	}

	public void setCellEditable(boolean isCellEditable)
	{
		this._isCellEditable = isCellEditable;
	}

	@Override
	public int getColumnCount()
	{
		return _columnNames.length;
	}

	@Override
	public int getRowCount()
	{
		return _data.length;
	}

	@Override
	public Object getValueAt(int r, int c)
	{
		return _data[r][c];
	}

	@Override
	public boolean isCellEditable(int row, int col)
	{
		return this._isCellEditable;
	}

	@Override
	public void setValueAt(Object value, int row, int col)
	{
		_auditSvc.addAudit(_tableName + Constant.DASH + row + Constant.DASH + col, String.valueOf(getValueAt(row, col)),
				String.valueOf(value));
		_data[row][col] = value;
		fireTableCellUpdated(row, col);
	}

	@Override
	public String getColumnName(int columnIndex)
	{
		String colName = "";
		if(columnIndex <= getColumnCount())
		{
			colName = _columnNames[columnIndex];
		}
		return colName;
	}

	@Override
	public Class getColumnClass(int c)
	{
		return getValueAt(0, c).getClass();
	}

	public String getTableName()
	{
		return _tableName;
	}

	public void setTableName(String tableName)
	{
		this._tableName = tableName;
	}
}

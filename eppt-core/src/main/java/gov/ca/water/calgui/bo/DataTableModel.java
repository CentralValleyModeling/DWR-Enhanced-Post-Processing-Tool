/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
//! Root table model for tabular data in the GUI

import javax.swing.*;
import javax.swing.table.AbstractTableModel;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IAuditSvc;
import gov.ca.water.calgui.tech_service.impl.AuditSvcImpl;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;

/**
 * This class is used to hold the tables for the application.
 *
 * @author Mohan
 */
public class DataTableModel extends AbstractTableModel
{
	private static final Logger LOG = Logger.getLogger(DataTableModel.class.getName());
	/**
	 * see {@link AuditSvcImpl}
	 */
	private IAuditSvc auditSvc = AuditSvcImpl.getAuditSvcImplInstance();
	/**
	 * This will hold the value of the table name in this table.
	 */
	private String tableName = "";
	/**
	 * This will hold all the column names in this table.
	 */
	private String[] columnNames;
	/**
	 * This will hold all the data values that will be displayed in the table.
	 */
	private Object[][] data;
	/**
	 * This will tell whether the table can be modify or not.
	 */
	private boolean isCellEditable;
	/**
	 * The object of the GUI.
	 */
	private SwingEngine swingEngine;

	public DataTableModel(String tableName, String[] columnName, Object[][] data, boolean isCellEditable)
	{
		this.tableName = tableName;
		this.columnNames = columnName;
		this.data = data;
		this.isCellEditable = isCellEditable;
	}

	@Override
	public Object clone() throws CloneNotSupportedException
	{
		String tableName = this.tableName;
		String[] colNames = new String[this.columnNames.length];
		for(int i = 0; i < colNames.length; i++)
		{
			colNames[i] = this.columnNames[i];
		}
		Object[][] data1 = new Object[this.data.length][this.data[0].length];
		for(int i = 0; i < data1.length; i++)
		{
			for(int j = 0; j < data1[0].length; j++)
			{
				data1[i][j] = this.data[i][j];
			}
		}
		return new DataTableModel(tableName, colNames, data1, this.isCellEditable);
	}

	public String[] getColumnNames()
	{
		return columnNames;
	}

	public void setColumnNames(String[] columnNames)
	{
		this.columnNames = columnNames;
	}

	public Object[][] getData()
	{
		return data;
	}

	public void setData(Object[][] data)
	{
		this.data = data;
	}

	public boolean isCellEditable()
	{
		return isCellEditable;
	}

	public void setCellEditable(boolean isCellEditable)
	{
		this.isCellEditable = isCellEditable;
	}

	@Override
	public int getColumnCount()
	{
		return columnNames.length;
	}

	@Override
	public int getRowCount()
	{
		return data.length;
	}

	@Override
	public Object getValueAt(int r, int c)
	{
		return data[r][c];
	}

	@Override
	public boolean isCellEditable(int row, int col)
	{
		return this.isCellEditable;
	}

	@Override
	public void setValueAt(Object value, int row, int col)
	{
		auditSvc.addAudit(tableName + Constant.DASH + row + Constant.DASH + col, String.valueOf(getValueAt(row, col)),
				String.valueOf(value));
		if(tableName.toLowerCase().startsWith("wsi") || tableName.equals(Constant.USER_DEFINED))
		{
			try
			{
				JLabel jLabel = (JLabel) swingEngine.find("op_WSIDI_Status");
				String oldValue = jLabel.getText();
				if(oldValue.endsWith(Constant.UNEDITED_FORLABEL))
				{
					String val = oldValue.replace(Constant.UNEDITED_FORLABEL, Constant.EDITED_FORLABEL);
					jLabel.setText(val);
				}
				else if(oldValue.endsWith("iterations)"))
				{
					String[] parts = oldValue.split("\\(");
					String val = parts[0] + Constant.EDITED_FORLABEL;
					jLabel.setText(val);
				}
			}
			catch(NullPointerException ex)
			{
				LOG.error(ex);
			}
		}
		data[row][col] = value;
		fireTableCellUpdated(row, col);
	}

	@Override
	public String getColumnName(int columnIndex)
	{
		String colName = "";
		if(columnIndex <= getColumnCount())
		{
			colName = columnNames[columnIndex];
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
		return tableName;
	}

	public void setTableName(String tableName)
	{
		this.tableName = tableName;
	}

	public void setSwingEngine(SwingEngine swingEngine)
	{
		this.swingEngine = swingEngine;
	}
}
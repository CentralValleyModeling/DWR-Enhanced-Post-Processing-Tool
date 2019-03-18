/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import javax.swing.table.AbstractTableModel;

/**
 * This class displays the session information as a table by implementing the
 * TableModel. The table columns are the parts of the pathname and the ordering
 * can be shuffled.
 * 
 * @author Nicky Sandhu
 * @version $Id: SessionTableModel.java,v 1.1 2003/10/02 20:49:32 redwood Exp $
 */
public class SessionTableModel extends AbstractTableModel {
	/**
	 * Construct a table
	 */
	public SessionTableModel(Session ss) {
		_session = ss;
	}

	/**
	 * returns the number of rows or size of data...
	 */
	public int getRowCount() {
		return _session.getNumberOfGroups();
	}

	/**
	 * The number of columns
	 */
	public int getColumnCount() {
		return 2;
	}

	/**
	 * returns name of column for given index
	 */
	public String getColumnName(int columnIndex) {
		if (columnIndex == 0)
			return "No.";
		else
			return "GROUP NAME";
	}

	/**
	 * returns value of object at row, column.
	 */
	public Object getValueAt(int rowIndex, int columnIndex) {
		if (columnIndex == 0)
			return "" + (rowIndex + 1);
		else
			return _session.getGroup(rowIndex).getName();
	}

	/**
	 * set value to aValue for row and column
	 */
	public void setValueAt(Object aValue, int rowIndex, int columnIndex) {
		if (columnIndex == 0) {
		} else {
			_session.getGroup(rowIndex).setName((String) aValue);
		}
	}

	/**
	 * returns true for all cells
	 */
	public boolean isCellEditable(int rowIndex, int columnIndex) {
		return false;
	}

	/**
	 * the session
	 */
	private Session _session;
}

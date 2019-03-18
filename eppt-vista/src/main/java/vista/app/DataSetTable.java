/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package vista.app;

import java.awt.BorderLayout;
import java.awt.Color;
import javax.swing.*;
import javax.swing.event.TableModelEvent;

import vista.gui.ExcelAdapter;
import vista.gui.VistaUtils;
import vista.set.DataRetrievalException;
import vista.set.DataSet;
import vista.set.FlagUtils;

@SuppressWarnings("serial")
public class DataSetTable extends JPanel {
	public DataSetTable(DataSet dataSet) {
		try {
			_dataModel = new DataSetTableModel(dataSet);
		} catch (DataRetrievalException dre) {
			VistaUtils.displayException(this._table, dre);
		}
		_table = new JTable(_dataModel);
		_table.setGridColor(Color.blue);
		_table.setVisible(true);
		_table.getTableHeader().addMouseMotionListener(
				new TableHeaderToolTipRenderer(_table, ""));
		_table.setShowVerticalLines(true);
		_table.setShowHorizontalLines(true);
		new ExcelAdapter(_table);

		//
		// editor for flags
		JComboBox flagEditor = new JComboBox();
		flagEditor.addItem("                ");
		flagEditor.addItem(FlagUtils
				.getQualityFlagName(FlagUtils.UNSCREENED_FLAG));
		flagEditor.addItem(FlagUtils.getQualityFlagName(FlagUtils.OK_FLAG));
		flagEditor.addItem(FlagUtils
				.getQualityFlagName(FlagUtils.QUESTIONABLE_FLAG));
		flagEditor.addItem(FlagUtils.getQualityFlagName(FlagUtils.REJECT_FLAG));
		DefaultCellEditor dce = new DefaultCellEditor(flagEditor);
		try {
			_table.getColumn("Flag Value").setCellEditor(dce);
		} catch (IllegalArgumentException ex) {
		}

		this.setLayout(new BorderLayout());
		this.add(new JScrollPane(_table));

	}

	/**
	 * show flag
	 */
	public void toggleFlagDisplay() {
		if (_dataModel.isFlagDisplayed())
			_dataModel.setFlagDisplayed(false);
		else
			_dataModel.setFlagDisplayed(true);
	}

	public int getColumnCount() {
		return _dataModel.getColumnCount();
	}

	public int getRowCount() {
		return _dataModel.getColumnCount();
	}

	public Object getValueAt(int row, int column) {
		return _table.getValueAt(row, column);
	}
	/**
	 * 
	 * @param flagType
	 */
	public void markAs(int flagType) {
		int[] rows = _table.getSelectedRows();
		if (rows == null)
			return;
		if (rows.length <= 0)
			return;
		for (int i = 0; i < rows.length; i++) {
			if (flagType == FlagUtils.MISSING_FLAG) {
				Object value = _table.getValueAt(rows[i], 1);
				if (!value.equals(DataSetTableModel.MV)
						&& !value.equals(DataSetTableModel.MR)) {
					continue;
				}
			}
			_table.setValueAt(FlagUtils.getQualityFlagName(flagType), rows[i],
					2);
		}
		updateTable(rows[0] - 1, rows[rows.length - 1] + 1);
	}

	/**
	 * 
	 * @param beginRow
	 * @param endRow
	 */
	public void updateTable(int beginRow, int endRow) {
		_table.tableChanged(new TableModelEvent(_table.getModel(), beginRow,
				endRow));
		_table.repaint();
	}

	public void setFlagOverride(boolean override) {
		((DataSetTableModel) _table.getModel()).setFlagOveridden(override);
	}

	public boolean isFlagOverride() {
		return ((DataSetTableModel) _table.getModel()).isFlagOveridden();
	}

	public JTable getTable() {
		return _table;
	}

	/**
	 * the table
	 */
	private JTable _table;
	/**
	 * the data set table model
	 */
	private DataSetTableModel _dataModel;
}

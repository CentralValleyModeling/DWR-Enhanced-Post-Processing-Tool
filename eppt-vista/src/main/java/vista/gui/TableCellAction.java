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
package vista.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import javax.swing.*;
import javax.swing.table.TableColumnModel;

/**
 * A class that listens to mouse clicks on a table's header and forwards them to
 * an action
 * 
 * @author Nicky Sandhu
 * @version $Id: TableCellAction.java,v 1.2 2000/03/21 18:16:30 nsandhu Exp $
 */
public abstract class TableCellAction extends MouseAdapter implements
		ActionListener {
	/**
    *
    */
	private JTable _table;
	private int _column, _row;
	private MouseEvent _me;
	private int _met;

	/**
	 * an action to be taken when a cell in the table is clicked
	 */
	public TableCellAction(JTable table) {
		this(table, MouseEvent.MOUSE_CLICKED);
	}

	/**
	 * an action to be taken when a cell in the table gets the desired type of
	 * mouse event
	 */
	public TableCellAction(JTable table, int mouseEventType) {
		_table = table;
		_table.addMouseListener(this);
		_column = -1;
		_row = -1;
		_met = mouseEventType;
	}

	/**
    *
    */
	private void setColumnAndRow(MouseEvent e) {
		TableColumnModel columnModel = _table.getColumnModel();
		int viewColumn = columnModel.getColumnIndexAtX(e.getX());
		_column = _table.convertColumnIndexToModel(viewColumn);
		_row = _table.rowAtPoint(e.getPoint());
		_me = e;
	}

	/**
    *
    */
	public void mouseClicked(MouseEvent e) {
		if (_met != MouseEvent.MOUSE_CLICKED)
			return;
		setColumnAndRow(e);
		if (_row == -1)
			return;
		if (e.getClickCount() > 1 && _column != -1) {
			// take action here...
			actionPerformed(new ActionEvent(e.getSource(),
					ActionEvent.ACTION_PERFORMED, "table cell clicked"));
		}
	}

	/**
    *
    */
	public void mouseMoved(MouseEvent e) {
		if (_met != MouseEvent.MOUSE_MOVED)
			return;
		setColumnAndRow(e);
		if (_row == -1)
			return;
		if (e.getClickCount() == 1 && _column != -1) {
			// take action here...
			actionPerformed(new ActionEvent(e.getSource(),
					ActionEvent.ACTION_PERFORMED, "table cell movement"));
		}
	}

	/**
    *
    */
	public int getColumn() {
		return _column;
	}

	/**
    *
    */
	public int getRow() {
		return _row;
	}

	/**
    *
    */
	public MouseEvent getMouseEvent() {
		return _me;
	}

	/**
    *
    */
	public abstract void actionPerformed(ActionEvent evt);
}

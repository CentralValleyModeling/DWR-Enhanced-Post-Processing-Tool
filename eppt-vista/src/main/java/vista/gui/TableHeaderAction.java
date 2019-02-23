/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
 * @version $Id: TableHeaderAction.java,v 1.2 2000/03/21 18:16:31 nsandhu Exp $
 */
public abstract class TableHeaderAction extends MouseAdapter implements
		ActionListener {
	/**
    *
    */
	private JTable _table;
	private int _column;
	private MouseEvent _me;

	/**
    *
    */
	public TableHeaderAction(JTable table) {
		_table = table;
		_table.getTableHeader().addMouseListener(this);
		_column = -1;
	}

	/**
    *
    */
	public void mouseClicked(MouseEvent e) {
		TableColumnModel columnModel = _table.getColumnModel();
		int viewColumn = columnModel.getColumnIndexAtX(e.getX());
		_column = _table.convertColumnIndexToModel(viewColumn);
		_me = e;
		if (e.getClickCount() == 1 && _column != -1) {
			// take action here...
			actionPerformed(new ActionEvent(e.getSource(),
					ActionEvent.ACTION_PERFORMED, "table header click"));
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
	public MouseEvent getMouseEvent() {
		return _me;
	}

	/**
    *
    */
	public abstract void actionPerformed(ActionEvent evt);
}

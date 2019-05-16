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

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Window;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import javax.swing.*;
import javax.swing.table.TableColumnModel;

/**
 * This class displays a window when the mouse is positioned on the cell of a
 * JTable
 */
public class TableCellToolTip implements MouseMotionListener, MouseListener
{
	private JTable _table;
	private Window _window;
	private JLabel _label;

	public TableCellToolTip(JTable table)
	{
		_table = table;
		_table.addMouseMotionListener(this);
		_table.addMouseListener(this);
		_window = new Window(VistaUtils.getFrameForComponent(_table));
		// create a jlabel and add it to jwindow
		_label = new JLabel();
		_window.add(_label);
	}

	public void mouseDragged(MouseEvent evt)
	{
		// do nothing
		_window.setVisible(false);
	}

	// show window placed exactly on cell
	public void mouseMoved(MouseEvent evt)
	{
		if(_window.isVisible())
		{
			return;
		}
		// get cell under this position
		TableColumnModel columnModel = _table.getColumnModel();
		int viewColumn = columnModel.getColumnIndexAtX(evt.getX());
		int column = _table.convertColumnIndexToModel(viewColumn);
		int row = _table.rowAtPoint(evt.getPoint());
		// set the text to be displayed
		_label.setText(_table.getValueAt(row, column).toString());
		// make the window of the cell's dimensions
		Rectangle rect = _table.getCellRect(row, column, true);
		_window.setSize(Math.max(rect.width, _label.getPreferredSize().width),
				rect.height);
		// place it right over the cell
		Point loc = _table.getLocationOnScreen();
		_window.setLocation(loc.x + rect.x, loc.y + rect.y);
		// show the window
		_window.setVisible(true);
	}

	//
	public void mouseClicked(MouseEvent evt)
	{
		_window.setVisible(false);
	}

	public void mouseEntered(MouseEvent evt)
	{
		_window.setVisible(true);
	}

	public void mouseExited(MouseEvent evt)
	{
		_window.setVisible(false);
	}

	public void mousePressed(MouseEvent evt)
	{
		_window.setVisible(false);
	}

	public void mouseReleased(MouseEvent evt)
	{
		_window.setVisible(false);
	}
}

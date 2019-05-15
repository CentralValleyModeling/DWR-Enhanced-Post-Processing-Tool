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
package vista.app;

import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Enables user to drag a set of rows or row
 *
 * @author Nicky Sandhu
 * @version $Id: RowMoveListener.java,v 1.1 2003/10/02 20:48:40 redwood Exp $
 */
public class RowMoveListener extends MouseAdapter
{
	private static final boolean DEBUG = false;
	/**
	 *
	 */
	private RowMovable _table;
	/**
	 *
	 */
	private int _irow = -1;

	/**
	 *
	 */
	public RowMoveListener(RowMovable table)
	{
		_table = table;
	}

	/**
	 * notes the row at which mouse was clicked
	 */
	public void mousePressed(MouseEvent e)
	{
		int row = _table.rowAtPoint(e.getPoint());
		if(e.getClickCount() == 1 && row != -1)
		{
			int shiftPressed = e.getModifiers() & InputEvent.SHIFT_MASK;
			if(shiftPressed == 0)
			{
				return;
			}
			if(DEBUG)
			{
				System.out.println("Shifting row # " + row);
			}
			_irow = row;
		}
	}

	/**
	 * notes the row at which mouse was released and transfers the row to that
	 * point.
	 */
	public void mouseReleased(MouseEvent e)
	{
		int row = _table.rowAtPoint(e.getPoint());
		if(DEBUG)
		{
			System.out.println("Mouse released @ " + row);
		}
		if(row != -1)
		{
			if(DEBUG)
			{
				System.out
						.println("Shifted row# " + _irow + " to row # " + row);
			}
			if(_irow < 0)
			{
				return;
			}

			_table.moveRow(_irow, row);
		}
		_irow = -1;
	}
}

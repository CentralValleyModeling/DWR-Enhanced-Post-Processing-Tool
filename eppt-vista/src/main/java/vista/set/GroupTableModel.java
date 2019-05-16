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
package vista.set;

import javax.swing.table.AbstractTableModel;

import vista.time.TimeWindow;

/**
 * This class displays the group information as a table by implementing the
 * TableModel. The table columns are the parts of the pathname and the ordering
 * can be shuffled.
 *
 * @author Nicky Sandhu
 * @version $Id: GroupTableModel.java,v 1.1 2003/10/02 20:49:24 redwood Exp $
 */
public class GroupTableModel extends AbstractTableModel
{
	/**
	 * the group
	 */
	private Group _group;

	/**
	 * Construct a table
	 */
	public GroupTableModel(Group g)
	{
		_group = g;
	}

	/**
	 * returns the number of rows or size of data...
	 */
	public int getRowCount()
	{
		return _group.getNumberOfDataReferences();
	}

	/**
	 * The number of columns
	 */
	public int getColumnCount()
	{
		return Pathname.MAX_PARTS + 1;
	}

	/**
	 * returns name of column for given index
	 */
	public String getColumnName(int columnIndex)
	{
		if(columnIndex == 0)
		{
			return "No.";
		}
		else
		{
			return Pathname.getPartName(columnIndex - 1);
		}
	}

	/**
	 * returns value of object at row, column.
	 */
	public Object getValueAt(int rowIndex, int columnIndex)
	{
		if(columnIndex == 0)
		{
			return new Integer(rowIndex + 1).toString();
		}
		columnIndex = columnIndex - 1;
		if(columnIndex != Pathname.D_PART)
		{
			// FIXME: can throw null pointer exception.
			if(_group.getDataReference(rowIndex).getPathname() != null)
			{
				return _group.getDataReference(rowIndex).getPathname().getPart(
						columnIndex);
			}
			else
			{
				return "";
			}
		}
		else
		{
			TimeWindow tw = _group.getDataReference(rowIndex).getTimeWindow();
			if(tw != null)
			{
				return tw.toString();
			}
			else
			{
				return "";
			}
		}
	}

	/**
	 * set value to aValue for row and column
	 */
	public void setValueAt(Object aValue, int rowIndex, int columnIndex)
	{
	}

	/**
	 * allow only time window field ( D part ) to be editable
	 */
	public boolean isCellEditable(int rowIndex, int columnIndex)
	{
		return false;
	}
}

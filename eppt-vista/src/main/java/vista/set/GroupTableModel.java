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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
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
	private final List<DataReference> _combinedTwReferences = new ArrayList<>();
	/**
	 * the group
	 */

	/**
	 * Construct a table
	 */
	public GroupTableModel(Group group)
	{
		DataReference[] dataReferences = group.getAllDataReferences();
		NavigableMap<Pathname, List<DataReference>> ref = new TreeMap<>(Comparator.comparing(Pathname::getFullPath));
		for(DataReference dataReference : dataReferences)
		{
			Pathname pathname = Pathname.createPathname(dataReference.getPathname());
			pathname.setPart(Pathname.D_PART, "");
			List<DataReference> refsForPath = ref.computeIfAbsent(pathname, e -> new ArrayList<>());
			refsForPath.add(dataReference);
		}
		for(List<DataReference> entry : ref.values())
		{
			if(!entry.isEmpty())
			{
				DataReference expandedRef = entry.get(0);
				for(int i = 1; i < entry.size(); i++)
				{
					TimeWindow union = entry.get(i).getTimeWindow().union(expandedRef.getTimeWindow());
					expandedRef = DataReference.createExpanded(expandedRef, union);
				}
				_combinedTwReferences.add(expandedRef);
			}
		}
	}

	/**
	 * returns the number of rows or size of data...
	 */
	public int getRowCount()
	{
		return _combinedTwReferences.size();
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
		if(rowIndex < _combinedTwReferences.size())
		{
			DataReference dataReference = _combinedTwReferences.get(rowIndex);
			if(dataReference != null)
			{
				if(columnIndex == Pathname.D_PART)
				{
					return dataReference.getTimeWindow();
				}
				else
				{
					return dataReference.getPathname().getPart(columnIndex);
				}
			}
		}
		return null;
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

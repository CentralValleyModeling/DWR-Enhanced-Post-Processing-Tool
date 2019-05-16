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

import java.util.Comparator;

public class ChainedComparators implements Comparator<DataReference>
{

	private Comparator<DataReference> primaryComparator;
	private Comparator<DataReference> secondaryComparator;

	public ChainedComparators(Comparator<DataReference> primaryComparator, Comparator<DataReference> secondaryComparator)
	{
		this.primaryComparator = primaryComparator;
		this.secondaryComparator = secondaryComparator;
	}

	@Override
	public int compare(DataReference o1, DataReference o2)
	{
		int value = primaryComparator.compare(o1, o2);
		if(value == 0)
		{
			return secondaryComparator.compare(o1, o2);
		}
		else
		{
			return value;
		}
	}

}

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

package calsim.msw;

import vista.set.DataReference;
import vista.set.DataSet;
import vista.time.TimeWindow;

public abstract class ReferenceOperator
{

	DataReference[] rIn;
	DataReference[] drIn;
	DataSet[] ds;

	protected ReferenceOperator(DataReference[] r)
	{
		rIn = r;
		ds = new DataSet[rIn.length];
	}

	protected void setTimeWindow(DataReference[] r, TimeWindow tw)
	{

		for(int i = 0; i < r.length; i++)
		{
			r[i] = DataReference.createExpanded(r[i], tw);
		}
	}

	protected abstract DataSet[] returnDataSetArray(TimeWindow tw);
}


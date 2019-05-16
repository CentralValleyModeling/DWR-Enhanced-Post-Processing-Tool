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

public class TransferOperator extends ReferenceOperator
{

	protected TransferOperator(DataReference[] r)
	{
		super(r);
	}

	//Method required by ReferenceOperator
	protected DataSet[] returnDataSetArray(TimeWindow tw)
	{
		setTimeWindow(rIn, tw);
		for(int i = 0; i < ds.length; i++)
		{
			ds[i] = rIn[i].getData();
		}
		return ds;
	}
}



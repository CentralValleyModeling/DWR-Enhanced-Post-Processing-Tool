/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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



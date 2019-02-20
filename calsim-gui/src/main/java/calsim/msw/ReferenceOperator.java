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


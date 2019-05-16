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
import vista.set.DataSetAttr;
import vista.set.RegularTimeSeries;
import vista.time.TimeFactory;
import vista.time.TimeInterval;
import vista.time.TimeWindow;

public class LumpOperator extends ReferenceOperator
{

	protected LumpOperator(DataReference[] r)
	{
		super(r);
	}

	//Method required by ReferenceOperator
	protected DataSet[] returnDataSetArray(TimeWindow tw)
	{

		setTimeWindow(rIn, tw);

		DataSet[] ds = new DataSet[rIn.length];
		TimeFactory tf = TimeFactory.getInstance();
		for(int i = 0; i < rIn.length; i++)
		{
			DataSet tDS = rIn[i].getData();

			double lumpedValue = 0;
			for(int j = 0; j < tDS.size(); j++)
			{
				lumpedValue = lumpedValue + tDS.getElementAt(j).getY();
			}
			lumpedValue = lumpedValue / tDS.size();

			double[] Y = {lumpedValue};
			DataSetAttr tDSA = tDS.getAttributes();
			int[] flag = {tDS.getElementAt(0).getFlag()};
			ds[i] = new RegularTimeSeries(tDS.getName(),
					tw.getEndTime(),
					tf.createTimeInterval(1, TimeInterval.MONTH_INTERVAL),
					Y,
					flag,
					tDSA);
		}
		for(int i = 0; i < ds.length; i++)
		{
		}
		return ds;
	}
}

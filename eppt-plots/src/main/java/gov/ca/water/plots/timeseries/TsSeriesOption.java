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

package gov.ca.water.plots.timeseries;

import gov.ca.water.plots.SeriesOption;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */

public class TsSeriesOption extends SeriesOption
{

	public TsSeriesOption(TimeSeriesContainer timeSeriesContainer)
	{
		super.setName(timeSeriesContainer.getFullName());
		int numberValues = timeSeriesContainer.getNumberValues();
		final Object[][] dateArray = new Object[numberValues][2];
		for(int i = 0; i < numberValues; i++)
		{
			HecTime hecTime = timeSeriesContainer.getHecTime(i);
			dateArray[i][0] = hecTime.getJavaDate(0);
			dateArray[i][1] = timeSeriesContainer.getValue(i);
		}
		super.setDataArray(dateArray);
	}
}

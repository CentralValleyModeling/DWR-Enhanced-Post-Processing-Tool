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

import java.util.List;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.junit.jupiter.api.Test;

import hec.heclib.util.HecTime;
import hec.heclib.util.HecTimeArray;
import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-23-2019
 */
public class TestTsPlotInput
{
	@Test
	public void testPlotInput() throws Exception
	{
		TsPlotInput tsPlotInput = new TsPlotInput();
		List<TsSeriesOption> tsSeriesOptions = tsPlotInput.getTsSeriesOption();

		TimeSeriesContainer tsc = new TimeSeriesContainer();
		HecTimeArray hecTimeArray = new HecTimeArray();
		HecTime time1 = new HecTime();
		time1.setDate("01/05/2019");
		time1.setTime("0000");
		hecTimeArray.setElementAt(time1, 0);
		HecTime time2 = new HecTime();
		time2.setDate("01 06 2019");
		time2.setTime("0000");
		hecTimeArray.setElementAt(time2, 1);
		tsc.set(new double[]{1.0, 50.0}, hecTimeArray);
		TsSeriesOption tsSeriesOption = new TsSeriesOption(tsc);
		tsSeriesOptions.add(tsSeriesOption);
		ObjectMapper mapper = new ObjectMapper();
//		mapper.enable(SerializationFeature.WRAP_ROOT_VALUE);
		System.out.println(mapper.writerWithDefaultPrettyPrinter().writeValueAsString(tsPlotInput));
	}
}

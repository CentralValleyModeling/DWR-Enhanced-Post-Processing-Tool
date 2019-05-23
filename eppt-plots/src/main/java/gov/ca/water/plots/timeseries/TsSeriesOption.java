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

import java.util.Arrays;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonRootName;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import gov.ca.water.plots.highchartsoptions.HighChartsLang;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
@JsonRootName("series")
public class TsSeriesOption
{
	private final String _name;
	private final double[][] _dateArray;

	public enum TsLineType
	{
		LINE("line");

		private final String _type;

		TsLineType(String type)
		{
			_type = type;
		}

		@Override
		public String toString()
		{
			return _type;
		}
	}

	private TsLineType _tsLineType = TsLineType.LINE;

	public TsSeriesOption(TimeSeriesContainer timeSeriesContainer)
	{
		_name = timeSeriesContainer.getShortName();
		int numberValues = timeSeriesContainer.getNumberValues();
		_dateArray = new double[2][numberValues];
		for(int i = 0; i < numberValues; i++)
		{
			HecTime hecTime = timeSeriesContainer.getHecTime(i);
			_dateArray[0][i] = hecTime.getJavaDate(0).getTime();
			_dateArray[1][i] = timeSeriesContainer.getValue(i);
		}
	}


	public static String toJson(TsSeriesOption tsSeriesOption) throws JsonProcessingException
	{
		ObjectMapper mapper = new ObjectMapper();
		mapper.enable(SerializationFeature.WRAP_ROOT_VALUE);
		return mapper.writeValueAsString(tsSeriesOption);
	}

	@JsonGetter
	public String getLineType()
	{
		return _tsLineType.toString();
	}

	@JsonGetter("name")
	public String getName()
	{
		return _name;
	}

	@JsonGetter("data")
	public String getData()
	{
		return Arrays.deepToString(_dateArray);
//		return Arrays.toString(_dateArray);
	}
}

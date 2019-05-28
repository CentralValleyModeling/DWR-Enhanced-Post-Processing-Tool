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

package gov.ca.water.plots;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import gov.ca.water.plots.timeseries.TsSeriesOption;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-27-2019
 */
public class SeriesOption
{
	private String _name;
	private Object[][] _dataArray;
	private TsStepType _step = TsStepType.NONE;
	private TsLineType _tsLineType = TsLineType.LINE;

	public enum TsLineType
	{
		LINE("line"), SPLINE("spline"), AREA("area");

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
	public enum TsStepType
	{
		BOP("left"), CENTER("center"), EOP("right"), NONE("null");

		private final String _type;


		TsStepType(String type)
		{
			_type = type;
		}
		@Override
		public String toString()
		{
			return _type;
		}

	}

	public void setName(String name)
	{
		_name = name;
	}

	public void setDataArray(Object[][] dataArray)
	{
		_dataArray = dataArray;
	}

	public static String toJson(TsSeriesOption tsSeriesOption) throws JsonProcessingException
	{
		ObjectMapper mapper = new ObjectMapper();
		mapper.enable(SerializationFeature.WRAP_ROOT_VALUE);
		return mapper.writeValueAsString(tsSeriesOption);
	}

	@JsonGetter("type")
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
	public Object[][] getData()
	{
		return _dataArray;
	}

	@JsonGetter("step")
	public String getStepType()
	{
		if(_step == TsStepType.NONE)
		{
			return null;
		}
		return _step.toString();
	}
}

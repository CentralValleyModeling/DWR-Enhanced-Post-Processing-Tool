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

package gov.ca.water.plots.highchartsoptions;

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.annotation.JsonRootName;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
@JsonRootName("lang")
public class HighChartsLang
{
	private String _decimalPoint = ".";
	private String _thousandsSep = ",";

	public static String toJson(HighChartsLang highChartsLang) throws JsonProcessingException
	{
		ObjectMapper mapper = new ObjectMapper();
		mapper.enable(SerializationFeature.WRAP_ROOT_VALUE);
		return mapper.writeValueAsString(highChartsLang);
	}

	@JsonGetter("decimalPoint")
	public String getDecimalPoint()
	{
		return _decimalPoint;
	}

	public void setDecimalPoint(String decimalPoint)
	{
		_decimalPoint = decimalPoint;
	}

	@JsonGetter("thousandsSep")
	public String getThousandsSep()
	{
		return _thousandsSep;
	}

	public void setThousandsSep(String thousandsSep)
	{
		_thousandsSep = thousandsSep;
	}
}

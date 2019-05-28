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

import com.fasterxml.jackson.annotation.JsonGetter;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
public class TsPlotInput
{
	private final TsChartOption _tsChartOption = new TsChartOption();
	private final TsTitleOption _tsTitleOption = new TsTitleOption();
	private final TsSubTitleOption _subTitleOption = new TsSubTitleOption();
	private final TsXAxisOption _tsXAxisOption = new TsXAxisOption();
	private final TsYAxisOption _tsYAxisOption = new TsYAxisOption();
	private final TsTimeOption _tsTimeOption = new TsTimeOption();
	private final TsLegendOption _tsLegendOption = new TsLegendOption();
	private final TsSeriesArrayOption _tsSeriesOption = new TsSeriesArrayOption();
	private final CreditsOption _creditsOption = new CreditsOption();

	@JsonGetter("chart")
	public TsChartOption getTsChartOption()
	{
		return _tsChartOption;
	}

	@JsonGetter("legend")
	public TsLegendOption getTsLegendOption()
	{
		return _tsLegendOption;
	}

	@JsonGetter("series")
	public List<TsSeriesOption> getTsSeriesOption()
	{
		return _tsSeriesOption.getTsSeriesOptions();
	}

	@JsonGetter("subtitle")
	public TsSubTitleOption getSubTitleOption()
	{
		return _subTitleOption;
	}

	@JsonGetter("time")
	public TsTimeOption getTsTimeOption()
	{
		return _tsTimeOption;
	}

	@JsonGetter("title")
	public TsTitleOption getTsTitleOption()
	{
		return _tsTitleOption;
	}

	@JsonGetter("xAxis")
	public TsXAxisOption getTsXAxisOption()
	{
		return _tsXAxisOption;
	}

	@JsonGetter("yAxis")
	public TsYAxisOption getTsYAxisOption()
	{
		return _tsYAxisOption;
	}

	@JsonGetter("credits")
	public CreditsOption getCreditsOption()
	{
		return _creditsOption;
	}

	public static String toJson(TsPlotInput tsPlotInput) throws JsonProcessingException
	{
		ObjectMapper mapper = new ObjectMapper();
		mapper.enable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
		return mapper.writeValueAsString(tsPlotInput);
	}
}

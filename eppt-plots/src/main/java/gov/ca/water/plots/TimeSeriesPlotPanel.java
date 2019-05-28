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

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import gov.ca.water.plots.highchartsoptions.HighChartsLang;
import gov.ca.water.plots.timeseries.TsPlotInput;
import gov.ca.water.plots.timeseries.TsSeriesOption;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;

import hec.io.TimeSeriesContainer;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-22-2019
 */
public class TimeSeriesPlotPanel extends HighChartsPanel
{
	private static final Logger LOGGER = Logger.getLogger(TimeSeriesPlotPanel.class.getName());
	private static final String OPTIONS = "Highcharts.setOptions({options});";
	private static final String CHART = "Highcharts.chart('container',{container});";
	private final List<TimeSeriesContainer> _timeSeriesContainers;
	private final TsPlotInput _input;

	public TimeSeriesPlotPanel(List<TimeSeriesContainer> timeSeriesContainers)
	{
		_timeSeriesContainers = timeSeriesContainers;
		_input = buildDefaultInput();
	}

	public TimeSeriesPlotPanel(List<TimeSeriesContainer> timeSeriesContainers, TsPlotInput input)
	{
		_timeSeriesContainers = timeSeriesContainers;
		_input = input;
	}

	private TsPlotInput buildDefaultInput()
	{
		TsPlotInput tsPlotInput = new TsPlotInput();
		for(TimeSeriesContainer tsc : _timeSeriesContainers)
		{
			tsPlotInput.getTsSeriesOption().add(new TsSeriesOption(tsc));
		}
		Map<String, List<TimeSeriesContainer>> collect = _timeSeriesContainers.stream().collect(Collectors.groupingBy(TimeSeriesContainer::getType));
		Set<String> strings = collect.keySet();
		for(String type : strings)
		{
			tsPlotInput.getTsYAxisOption().getTitleOption().setTitle(type);
		}
		return tsPlotInput;
	}

	@Override
	void initFx()
	{
		BorderPane borderPane = new BorderPane();
		String callback = "";
		try
		{
			callback = OPTIONS.replace("{options}", HighChartsLang.toJson(new HighChartsLang()));
			callback += CHART.replace("{container}", TsPlotInput.toJson(_input));
		}
		catch(JsonProcessingException e)
		{
			LOGGER.log(Level.SEVERE, "error stylizing plots");
		}
		JavascriptPanel javascriptPanel = new JavascriptPanel("gov/ca/water/plots/timeseries/timeseries.html", callback);
		borderPane.setCenter(javascriptPanel);
		Scene scene = new Scene(borderPane);
		setScene(scene);
	}
}

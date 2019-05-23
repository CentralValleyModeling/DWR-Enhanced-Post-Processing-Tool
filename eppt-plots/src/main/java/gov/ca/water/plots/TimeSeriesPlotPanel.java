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

import java.util.logging.Level;
import java.util.logging.Logger;

import com.fasterxml.jackson.core.JsonProcessingException;
import gov.ca.water.plots.highchartsoptions.HighChartsLang;
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
	private final TimeSeriesContainer[] _timeSeriesContainers;

	public TimeSeriesPlotPanel(TimeSeriesContainer[] timeSeriesContainers)
	{
		_timeSeriesContainers = timeSeriesContainers;
	}

	@Override
	void initFx()
	{
		BorderPane borderPane = new BorderPane();
		String callback = "";
		try
		{
			callback = OPTIONS.replace("{options}", HighChartsLang.toJson(new HighChartsLang()));
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

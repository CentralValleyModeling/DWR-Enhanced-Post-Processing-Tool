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

package gov.ca.water.plotly;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.channels.ClosedByInterruptException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.json.JSONArray;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-19-2019
 */
public abstract class PlotlyChart
{
	public JSONObject buildJSON() throws PlotlyPrintException
	{
		try
		{
			JSONObject jsonObject = new JSONObject(getJSONTemplate());
			JSONObject layout = buildLayout(jsonObject.getJSONObject("layout"));
			JSONArray dataArray = buildDataArray(jsonObject.getJSONArray("data"));
			jsonObject.put("data", dataArray);
			jsonObject.put("layout", layout);
			return jsonObject;
		}
		catch(UncheckedIOException e)
		{
			if(e.getCause() instanceof ClosedByInterruptException)
			{
				throw new PlotlyPrintException("Task Interrupted");
			}
			else
			{
				throw new PlotlyPrintException("Unable to build JSON object", e);
			}
		}
		catch(IOException | RuntimeException e)
		{
			throw new PlotlyPrintException("Unable to build JSON object for " + getPlotType(), e);
		}
	}

	private String getJSONTemplate() throws IOException
	{
		try(Stream<String> lines = Files.lines(getTemplatePath()))
		{
			return lines.collect(Collectors.joining());
		}
	}

	protected abstract Path getTemplatePath();

	protected abstract JSONObject buildLayout(JSONObject template);

	protected abstract JSONArray buildDataArray(JSONArray arrayTemplate);

	public abstract String getPlotType();
}

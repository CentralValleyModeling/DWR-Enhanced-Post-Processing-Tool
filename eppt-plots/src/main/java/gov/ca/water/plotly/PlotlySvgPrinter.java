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

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-19-2019
 */
public final class PlotlySvgPrinter
{
	private static final Logger LOGGER = Logger.getLogger(PlotlySvgPrinter.class.getName());

	private PlotlySvgPrinter()
	{
		throw new AssertionError("Utility class");
	}

	public static void printSvgToPath(Path path, PlotlyChart plotlyChart) throws PlotlyPrintException
	{
		JSONObject jsonObject = plotlyChart.buildJSON();
		printSvg(path, jsonObject, plotlyChart.getWidth(), plotlyChart.getHeight());
	}

	static void printSvg(Path path, JSONObject jsonObject, int width, int height) throws PlotlyPrintException
	{
		try
		{
			Path jsonPath = Paths.get(path.getFileName().toString().replace(".svg", ".json"));
			Path jsonFilePath = path.toAbsolutePath().getParent()
									.resolve(jsonPath);
			Path parent = jsonFilePath.getParent();
			if(!parent.toFile().exists())
			{
				boolean mkdirs = parent.toFile().mkdirs();
				if(!mkdirs)
				{
					throw new PlotlyPrintException("Unable to create images directory: " + parent);
				}
			}
			try(BufferedWriter writer = Files.newBufferedWriter(jsonFilePath))
			{
				jsonObject.write(writer);
			}
			LOGGER.log(Level.FINE, "Plotting object: {0}", jsonObject);
		}
		catch(IOException | RuntimeException e)
		{
			throw new PlotlyPrintException("Unable to create plot: " + path, e);
		}
	}
}

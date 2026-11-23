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
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import gov.ca.water.calgui.constant.Constant;
import org.json.JSONObject;

import static java.util.stream.Collectors.toList;

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

	public static void printJsonToPath(Path path, PlotlyChart plotlyChart) throws PlotlyPrintException
	{
		JSONObject jsonObject = plotlyChart.buildJSON();
		printJson(path, jsonObject);
	}

	static void printJson(Path path, JSONObject jsonObject) throws PlotlyPrintException
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

	/**
	 * Renders every plotly JSON file in the directory to a same-named SVG file.
	 */
	public static void printSvg(Path projectImageDir) throws PlotlyPrintException
	{
		printSvg(projectImageDir, Paths.get(Constant.CONFIG_DIR).getParent().resolve("lib").resolve("plotly").resolve("dist").resolve("plotly.min.js"));
	}

	static void printSvg(Path imageDir, Path plotlyJs) throws PlotlyPrintException
	{
		List<Path> jsonFiles;
		try(Stream<Path> files = Files.walk(imageDir, 1))
		{
			jsonFiles = files.filter(p -> p.toString().endsWith(".json"))
					.sorted()
					.collect(toList());
		}
		catch(IOException e)
		{
			throw new PlotlyPrintException("Unable to list plotly JSON files in: " + imageDir, e);
		}
		LOGGER.log(Level.INFO, "Plotly SVG generation: {0} images to render", jsonFiles.size());
		if(jsonFiles.isEmpty())
		{
			return;
		}
		try
		{
			new WebViewSvgRenderer(plotlyJs).render(jsonFiles);
		}
		catch(InterruptedException e)
		{
			Thread.currentThread().interrupt();
			throw new PlotlyPrintException("Plotly SVG generation process interrupted", e);
		}
	}
}

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
import java.nio.file.StandardCopyOption;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import gov.ca.water.calgui.constant.Constant;
import org.apache.commons.io.FileUtils;
import org.json.JSONObject;

import static gov.ca.water.calgui.constant.Constant.ORCA_EXE;
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

	public static void printSvg(Path projectImageDir) throws PlotlyPrintException
	{
		try
		{
			//Need to do Plotly SVG generation in temp directory because of bug in Plotly generating inside OneDrive folder
			Path tempImageDir = Files.createTempDirectory("DWR-EPPT-" + projectImageDir.getFileName());
			try(Stream<Path> files = Files.walk(projectImageDir, 1))
			{
				List<Path> jsonFiles = files.filter(p -> p.toString().endsWith(".json"))
											.collect(toList());
				for(Path jsonPath : jsonFiles)
				{
					Files.copy(jsonPath, tempImageDir.resolve(jsonPath.getFileName()), StandardCopyOption.REPLACE_EXISTING);
				}
			}
			try(Stream<Path> files = Files.walk(tempImageDir, 1))
			{
				String jsonFiles = files.filter(p -> p.toString().endsWith(".json"))
										.map(tempImageDir.getParent()::relativize)
										.map(Path::toString)
										.map(s -> "\"" + s + "\"")
										.collect(Collectors.joining(" "));
				String orcaCommandline = "\"" + ORCA_EXE + "\" graph " + jsonFiles + " --format svg --output-dir \""
						+ tempImageDir.getFileName() + "\"" + " --plotly \""
						+ Paths.get(Constant.CONFIG_DIR).getParent().resolve("lib").resolve("plotly").resolve("dist").resolve("plotly.min.js") + "\"";
				LOGGER.log(Level.FINE, "Plotly SVG generation command line: {0}", orcaCommandline);
				Process exec = new ProcessBuilder()
						.directory(tempImageDir.getParent().toFile())
						.command(orcaCommandline)
						.start();
				exec.waitFor();
				int exitCode = exec.exitValue();
				if(exitCode != 0)
				{
					throw new PlotlyPrintException("Unable to create plots: " + tempImageDir + "\n Exit code: " + exitCode);
				}
			}
			try(Stream<Path> files = Files.walk(tempImageDir, 1))
			{
				List<Path> svgFiles = files.filter(p -> p.toString().endsWith(".svg"))
											.collect(toList());
				for(Path jsonPath : svgFiles)
				{
					Files.copy(jsonPath, projectImageDir.resolve(jsonPath.getFileName()), StandardCopyOption.REPLACE_EXISTING);
				}
			}
			FileUtils.deleteDirectory(tempImageDir.toFile());
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Error creating images directory", e);
		}
		catch(InterruptedException e)
		{
			Thread.currentThread().interrupt();
			throw new PlotlyPrintException("Plotly SVG generation process interrupted", e);
		}
	}


}

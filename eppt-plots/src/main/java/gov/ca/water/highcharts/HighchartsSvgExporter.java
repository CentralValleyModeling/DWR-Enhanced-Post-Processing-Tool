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

package gov.ca.water.highcharts;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javafx.application.Platform;
import org.apache.commons.io.FileUtils;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-05-2019
 */
public final class HighchartsSvgExporter
{
	private static final Logger LOGGER = Logger.getLogger(HighchartsSvgExporter.class.getName());

	private HighchartsSvgExporter()
	{
		throw new AssertionError("Utility class");
	}

	public static void printSvgToPath(Path path, ChartParameters chartParameters)
	{
		try
		{
			CountDownLatch countDownLatch = new CountDownLatch(5);
			CompletableFuture.runAsync(()-> printChart(path, chartParameters, countDownLatch), Platform::runLater);
			boolean await = countDownLatch.await(10L, TimeUnit.SECONDS);
			if(!await)
			{
				LOGGER.log(Level.SEVERE, "Timeout occured, SVG file might not have been generated: {0}", path);
			}
		}
		catch(InterruptedException e)
		{
			LOGGER.log(Level.SEVERE, "Countdown latch interrupted waiting for SVG file printing: " + path, e);
			Thread.currentThread().interrupt();
		}
	}

	private static void printChart(Path path, ChartParameters chartParameters, CountDownLatch countDownLatch)
	{
		ChartType chartType = chartParameters.getChartType();
		HighChartsSvgPrinter highChartsSvgPrinter = new HighChartsSvgPrinter();
		highChartsSvgPrinter.load(chartType.getChartPath(),()->printSvg(path, chartParameters, highChartsSvgPrinter, countDownLatch));
	}

	private static void printSvg(Path path, ChartParameters chartParameters, HighChartsSvgPrinter highChartsSvgPrinter,
								 CountDownLatch countDownLatch)
	{
		try
		{
			String svg = highChartsSvgPrinter.exportToSvgScript(chartParameters.getJsonObject());
			FileUtils.writeStringToFile(path.toFile(), svg, StandardCharsets.UTF_8.name());
		}
		catch(IOException | RuntimeException e)
		{
			LOGGER.log(Level.SEVERE, "Error exporting generated SVG file", e);
		}
		finally
		{
			countDownLatch.countDown();
		}
	}
}

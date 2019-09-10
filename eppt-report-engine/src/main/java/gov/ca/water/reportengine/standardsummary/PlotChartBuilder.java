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

package gov.ca.water.reportengine.standardsummary;

import java.nio.file.Path;
import java.util.List;
import java.util.regex.Pattern;

import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.PlotlyChart;
import gov.ca.water.plotly.PlotlyPrintException;
import gov.ca.water.plotly.PlotlySvgPrinter;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import static gov.ca.water.reportengine.EPPTReport.checkInterrupt;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-29-2019
 */
abstract class PlotChartBuilder extends StandardSummaryChartBuilder
{
	private static final Pattern DOUBLE_PIPE_PATTERN = Pattern.compile("\\|\\|");

	PlotChartBuilder(Document document, EpptScenarioRun base,
					 List<EpptScenarioRun> alternatives,
					 SummaryReportParameters reportParameters)
	{
		super(document, base, alternatives, reportParameters);
	}

	void buildChart(Path imagePath, Element retval, EpptChart epptChart) throws EpptReportException
	{
		List<ChartComponent> chartComponents = epptChart.getChartComponents();
		checkInterrupt();
		PlotlyChart plotlyExceedance = buildChart(chartComponents);
		retval.setAttribute(SVG_FILE_LOCATION_ATTRIBUTE, imagePath.toString());
		printChart(imagePath, plotlyExceedance);
	}

	abstract PlotlyChart buildChart(List<ChartComponent> chartComponents) throws EpptReportException;

	private void printChart(Path imagePath, PlotlyChart plotlyExceedance) throws EpptReportException
	{
		try
		{
			PlotlySvgPrinter.printJsonToPath(imagePath, plotlyExceedance);
		}
		catch(PlotlyPrintException e)
		{
			throw new EpptReportException("Unable to generate Exceedance Plot: " + imagePath, e);
		}
	}

	String getTitleForComponents(List<ChartComponent> chartComponents)
	{
		return chartComponents.stream().map(ChartComponent::getTitle).filter(s -> !s.isEmpty()).findAny().orElse("");
	}

	String getXAxisLabelForComponents(List<ChartComponent> chartComponents)
	{
		return chartComponents.stream()
							  .map(ChartComponent::getHeader)
							  .filter(s -> !s.isEmpty())
							  .map(DOUBLE_PIPE_PATTERN::split)
							  .map(s -> s[0])
							  .findAny()
							  .orElse("");
	}

	String getYAxisLabelForComponents(List<ChartComponent> chartComponents)
	{
		return chartComponents.stream()
							  .map(ChartComponent::getHeader)
							  .filter(s -> !s.isEmpty())
							  .map(DOUBLE_PIPE_PATTERN::split)
							  .filter(s -> s.length > 1)
							  .map(s -> s[1])
							  .findAny()
							  .orElse("");
	}
}

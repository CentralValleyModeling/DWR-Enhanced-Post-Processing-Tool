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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.plotly.PlotlyPrintException;
import gov.ca.water.plotly.PlotlySvgPrinter;
import gov.ca.water.reportengine.EpptReportException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import static gov.ca.water.calgui.constant.Constant.ORCA_EXE;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class StandardSummaryWriter
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private static final String SUMMARY_ELEMENT = "summary";
	private static final String MODULE_ELEMENT = "module";
	private static final String SECTION_ELEMENT = "section";
	private static final String SUB_MODULE_ELEMENT = "sub-module";
	private static final String MODULE_NAME_ELEMENT = "module-name";
	private static final String SECTION_NAME_ELEMENT = "section-name";
	private static final String SUB_MODULE_NAME_ELEMENT = "sub-module-name";
	private static final String MODULE_ORDER_ATTRIBUTE = "module-order";
	private static final String SECTION_ORDER_ATTRIBUTE = "section-order";
	private static final String SUB_MODULE_ORDER_ATTRIBUTE = "sub-module-order";
	private static final String CHART_ELEMENT = "chart";
	private static final String CHART_ORDER_ATTRIBUTE = "chart-order";
	private static final String CHART_NAME_ATTRIBUTE = "chart-name";
	private final Document _document;
	private final SummaryReportParameters _reportParameters;
	private final Path _imageDirectory;
	private final Path _rotatedImageDirectory;
	private final ListBuilder _listBuilder;
	private final BaseAltDiffTableBuilder _baseAltDiffTableBuilder;
	private final ScatterPlotBuilder _scatterPlotBuilder;
	private final CoaTableBuilder _coaTableBuilder;
	private final ControlTableBuilder _controlTableBuilder;
	private final ExceedanceChartBuilder _exceedanceChartBuilder;
	private final ExceedanceChartPageBuilder _exceedanceChartPageBuilder;
	private final LinePlotBuilder _linePlotBuilder;
	private final PercentDiffTableBuilder _percentDiffTableBuilder;

	public StandardSummaryWriter(Document document, EpptScenarioRun base, List<EpptScenarioRun> alternatives, SummaryReportParameters reportParameters,
								 Path imageDir)
	{
		_document = document;
		_reportParameters = reportParameters;
		_imageDirectory = imageDir.toAbsolutePath();
		_rotatedImageDirectory = _imageDirectory.resolve("rotated");
		_listBuilder = new ListBuilder(document, base, alternatives, reportParameters);
		_baseAltDiffTableBuilder = new BaseAltDiffTableBuilder(document, base, alternatives, reportParameters);
		_coaTableBuilder = new CoaTableBuilder(document, base, alternatives, reportParameters);
		_controlTableBuilder = new ControlTableBuilder(document, base, alternatives, reportParameters);
		_exceedanceChartBuilder = new ExceedanceChartBuilder(document, base, alternatives, reportParameters);
		_exceedanceChartPageBuilder = new ExceedanceChartPageBuilder(document, base, alternatives, reportParameters);
		_linePlotBuilder = new LinePlotBuilder(document, base, alternatives, reportParameters);
		_percentDiffTableBuilder = new PercentDiffTableBuilder(document, base, alternatives, reportParameters);
		_scatterPlotBuilder = new ScatterPlotBuilder(document, base, alternatives, reportParameters);
	}

	public Element write(List<EpptChart> charts) throws EpptReportException
	{
		Element retval = _document.createElement(SUMMARY_ELEMENT);
		List<String> moduleList = charts.stream()
										.map(EpptChart::getModule)
										.distinct()
										.collect(toList());
		for(int i = 0; i < moduleList.size(); i++)
		{
			String moduleName = moduleList.get(i);
			if(!_reportParameters.getDisabledSummaryModules().contains(moduleName))
			{
				Element module = writeModule(moduleName, charts);
				module.setAttribute(MODULE_ORDER_ATTRIBUTE, String.valueOf(i));
				retval.appendChild(module);
			}
		}

		printSvgPlots();
		return retval;
	}

	private void printSvgPlots() throws EpptReportException
	{
		try
		{
			PlotlySvgPrinter.printSvg(_imageDirectory);
			PlotlySvgPrinter.printSvg(_rotatedImageDirectory);
			rotateImages();
		}
		catch(PlotlyPrintException | IOException e)
		{
			throw new EpptReportException("Unable to print SVG files for JSON directory: " + _imageDirectory, e);
		}
	}

	private void rotateImages() throws IOException
	{
		List<Path> svgPaths;
		try(Stream<Path> stream = Files.walk(_rotatedImageDirectory.resolve("svg"), 1))
		{
			svgPaths = stream.filter(p->!p.toFile().isDirectory()).filter(p -> p.toString().endsWith("svg")).collect(toList());
		}
		for(Path path : svgPaths)
		{
			String newText;
			try(Stream<String> lines = Files.lines(path))
			{
				String collect = lines.collect(joining("\n"));
				newText = collect.replaceFirst("<svg", "<svg transform=\"rotate(90, 0, 150)\"");
			}
			Files.write(path, Collections.singleton(newText));
		}
	}

	private Element writeModule(String moduleName, List<EpptChart> charts)
	{
		LOGGER.at(Level.INFO).log("Standard Summary Statistics: Building XML for Module: %s", moduleName);
		Element retval = _document.createElement(MODULE_ELEMENT);
		retval.setAttribute(MODULE_NAME_ELEMENT, moduleName);
		List<String> sectionList = charts.stream()
										 .filter(c -> c.getModule().equals(moduleName))
										 .map(EpptChart::getSection).distinct().collect(toList());
		for(int i = 0; i < sectionList.size(); i++)
		{
			String sectionName = sectionList.get(i);
			Element section = writeSection(moduleName, sectionName, charts);
			section.setAttribute(SECTION_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(section);
		}
		return retval;
	}

	private Element writeSection(String moduleName, String sectionName, List<EpptChart> charts)
	{
		LOGGER.at(Level.INFO).log("Standard Summary Statistics: Building XML for section: %s", sectionName);
		Element retval = _document.createElement(SECTION_ELEMENT);
		retval.setAttribute(SECTION_NAME_ELEMENT, sectionName);
		List<String> subModuleList = charts.stream()
										   .filter(c -> c.getModule().equals(moduleName))
										   .filter(c -> c.getSection().equals(sectionName))
										   .map(EpptChart::getSubModule).distinct().collect(toList());
		for(int i = 0; i < subModuleList.size(); i++)
		{
			String subModuleName = subModuleList.get(i);
			Element subModule = writeSubModule(moduleName, sectionName, subModuleName, charts);
			subModule.setAttribute(SUB_MODULE_ORDER_ATTRIBUTE, String.valueOf(i));
			retval.appendChild(subModule);
		}
		return retval;
	}

	private Element writeSubModule(String moduleName, String sectionName, String subModuleName, List<EpptChart> charts)
	{
		LOGGER.at(Level.INFO).log("Standard Summary Statistics: Building XML for Sub-Module: %s", subModuleName);
		Element retval = _document.createElement(SUB_MODULE_ELEMENT);
		retval.setAttribute(SUB_MODULE_NAME_ELEMENT, subModuleName);
		List<String> chartIdList = charts.stream()
										 .filter(c -> c.getModule().equals(moduleName))
										 .filter(c -> c.getSection().equals(sectionName))
										 .filter(c -> c.getSubModule().equals(subModuleName))
										 .map(EpptChart::getChartId).distinct().collect(toList());
		for(int i = 0; i < chartIdList.size(); i++)
		{
			int index = i;
			String chartId = chartIdList.get(i);
			charts.stream()
				  .filter(c -> c.getModule().equals(moduleName))
				  .filter(c -> c.getSection().equals(sectionName))
				  .filter(c -> c.getSubModule().equals(subModuleName))
				  .filter(c -> c.getChartId().equals(chartId))
				  .findAny()
				  .flatMap(epptChart -> buildChart(epptChart, index))
				  .ifPresent(retval::appendChild);
		}
		return retval;
	}

	private Optional<Element> buildChart(EpptChart epptChart, int index)
	{
		String chartId = epptChart.getChartId();
		LOGGER.at(Level.INFO).log("Standard Summary Statistics: Building XML for chart: %s", chartId);
		Element retval = null;
		try
		{
			Element chart = _document.createElement(chartId);
			chart.setAttribute(CHART_ORDER_ATTRIBUTE, String.valueOf(index));
			ChartType chartType = epptChart.getChartType();
			switch(chartType)
			{
				case LIST:
					_listBuilder.buildList(chart, epptChart);
					break;
				case BASE_ALT_DIFF_TABLE:
					_baseAltDiffTableBuilder.buildTable(chart, epptChart);
					break;
				case COA_TABLE:
					_coaTableBuilder.buildTable(chart, epptChart);
					break;
				case CONTROL_TABLE:
					_controlTableBuilder.buildTable(chart, epptChart);
					break;
				case EXCEEDANCE:
					Path imagePath = _imageDirectory.resolve(chartId + ".svg");
					_exceedanceChartBuilder.buildChart(imagePath, chart, epptChart);
					break;
				case EXCEEDANCE_PAGE:
					imagePath = _rotatedImageDirectory.resolve(chartId + ".svg");
					_exceedanceChartPageBuilder.buildChart(imagePath, chart, epptChart);
					break;
				case LINE_PLOT:
					imagePath = _imageDirectory.resolve(chartId + ".svg");
					_linePlotBuilder.buildChart(imagePath, chart, epptChart);
					break;
				case PERCENT_DIFF_TABLE:
					_percentDiffTableBuilder.buildTable(chart, epptChart);
					break;
				case SCATTER_PLOT:
					imagePath = _imageDirectory.resolve(chartId + ".svg");
					_scatterPlotBuilder.buildChart(imagePath, chart, epptChart);
					break;
			}
			retval = _document.createElement(CHART_ELEMENT);
			retval.setAttribute(CHART_NAME_ATTRIBUTE, chartId);
			retval.appendChild(chart);
		}
		catch(RuntimeException | EpptReportException e)
		{
			LOGGER.at(Level.SEVERE).withCause(e).log("Error writing data for Chart ID: %s", epptChart);
		}
		return Optional.ofNullable(retval);
	}
}

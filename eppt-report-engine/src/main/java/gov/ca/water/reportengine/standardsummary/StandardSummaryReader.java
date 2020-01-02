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
import java.time.LocalDateTime;
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import javax.script.ScriptException;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.scripts.JythonScriptRunner;
import gov.ca.water.calgui.techservice.impl.FilePredicates;
import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.jython.JythonScriptBuilder;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class StandardSummaryReader
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private static final Pattern CSV_PATTERN = Pattern.compile(",");
	private static final int MODULE_INDEX = 0;
	private static final int SECTION_INDEX = 1;
	private static final int SUB_MODULE_INDEX = 2;
	private static final int CHART_TYPE_INDEX = 3;
	private static final int CHART_ID_INDEX = 4;
	private static final int TITLE_INDEX = 5;
	private static final int HEADER_INDEX = 6;
	private static final int SUB_HEADER_INDEX = 7;
	private static final int COMPONENT_INDEX = 8;
	private static final int FUNCTION_INDEX = 9;
	private final Path _csvPath;
	private final StandardSummaryErrors _standardSummaryErrors;

	public StandardSummaryReader(Path csvPath, StandardSummaryErrors standardSummaryErrors)
	{
		_csvPath = csvPath;
		_standardSummaryErrors = standardSummaryErrors;
	}

	public List<String> getOrderedChartIds() throws EpptReportException
	{
		try(Stream<String> lines = Files.lines(_csvPath))
		{
			return parseLines(lines)
					.map(line -> line[CHART_ID_INDEX])
					.collect(toList());
		}
		catch(IOException | RuntimeException ex)
		{
			throw new EpptReportException("Error processing Standard Summary Statistics configuration file: " + _csvPath, ex);
		}
	}


	public Map<String, EpptChart> readLines() throws EpptReportException
	{
		try(Stream<String> lines = Files.lines(_csvPath))
		{
			return parseLines(lines)
					.collect(groupingBy(line -> line[CHART_ID_INDEX], mapping(line -> line,
							collectingAndThen(toList(), this::linesToChart))));
		}
		catch(IOException | RuntimeException ex)
		{
			throw new EpptReportException("Error processing Standard Summary Statistics configuration file: " + _csvPath, ex);
		}
	}

	private Stream<String[]> parseLines(Stream<String> lines)
	{
		return lines.filter(FilePredicates.commentFilter()).map(CSV_PATTERN::split)
					.filter(s -> s.length >= 10)
					.map(this::replaceCommas);
	}

	private String[] replaceCommas(String[] input)
	{
		for(int i = 0; i < input.length; i++)
		{
			input[i] = input[i].replace("%2C", ",");
		}
		return input;
	}

	private EpptChart linesToChart(List<String[]> lines)
	{
		String[] firstLine = lines.get(0);
		List<ChartComponent> chartComponents = new ArrayList<>();
		for(String[] line : lines)
		{
			String title = attemptFunction(line[TITLE_INDEX]);
			String header = attemptFunction(line[HEADER_INDEX]);
			String subHeader = attemptFunction(line[SUB_HEADER_INDEX]);
			String component = attemptFunction(line[COMPONENT_INDEX]);
			ChartComponent chartComponent = new ChartComponent(title, header, subHeader, component,
					line[FUNCTION_INDEX]);
			chartComponents.add(chartComponent);
		}
		ChartType chartType;
		try
		{
			chartType = ChartType.getChartTypeForId(firstLine[CHART_TYPE_INDEX]);
		}
		catch(IllegalArgumentException e)
		{
			throw e;
		}
		catch(EpptReportException e)
		{
			throw new IllegalArgumentException("Error processing line: " + Arrays.toString(firstLine), e);
		}
		return new EpptChart(firstLine[MODULE_INDEX], firstLine[SECTION_INDEX], firstLine[SUB_MODULE_INDEX], chartType,
				firstLine[CHART_ID_INDEX], chartComponents);
	}

	private String attemptFunction(String reference)
	{
		Optional<String> script = JythonScriptBuilder.getInstance().getScript(reference);
		return script.map(this::runTitleScript).orElse(reference);
	}

	private String runTitleScript(String script)
	{
		String retval = script;
		JythonScriptRunner runner = new JythonScriptRunner(null, new CommonPeriodFilter(LocalDateTime.of(1850, Month.JANUARY, 1, 0, 0),
				LocalDateTime.of(2150, Month.JANUARY, 1, 0, 0)));
		try
		{
			retval = runner.runScript(script).toString();
		}
		catch(ScriptException | RuntimeException e)
		{
			_standardSummaryErrors.addError(LOGGER, "Error running title script: " + script, e);
		}
		return retval;
	}


	public List<String> getModules() throws EpptReportException
	{
		try(Stream<String> lines = Files.lines(_csvPath))
		{
			return parseLines(lines)
					.map(line -> line[MODULE_INDEX])
					.map(String::trim)
					.distinct()
					.collect(toList());
		}
		catch(IOException | RuntimeException ex)
		{
			throw new EpptReportException("Error processing Standard Summary Statistics configuration file: " + _csvPath, ex);
		}
	}
}

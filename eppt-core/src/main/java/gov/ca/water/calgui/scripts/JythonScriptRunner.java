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

package gov.ca.water.calgui.scripts;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.bo.AnnualPeriodFilter;
import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.bo.PeriodFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-05-2019
 */
public class JythonScriptRunner
{
	private static final ScriptEngine PYTHON_ENGINE = new ScriptEngineManager().getEngineByName("python");
	private final EpptScenarioRun _epptScenarioRun;
	private final WaterYearDefinition _waterYearDefinition;
	private final DssCache _dssCache;
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	private DssReader _dssReader;

	public JythonScriptRunner(EpptScenarioRun epptScenarioRun, CommonPeriodFilter commonPeriodFilter,
							  WaterYearDefinition waterYearDefinition, DssCache dssCache)
	{

		_epptScenarioRun = epptScenarioRun;
		_waterYearDefinition = waterYearDefinition;
		_dssCache = dssCache;
		if(PYTHON_ENGINE == null)
		{
			throw new IllegalArgumentException("Unable to find jython engine");
		}
		initializeGlobalVariables(commonPeriodFilter);
	}

	public static void initializeScriptDirectory()
	{
		try(Stream<Path> stream = Files.walk(Constant.QA_QC_SCRIPT_DIRECTORY, 1))
		{
			List<Path> collect = stream.filter(p -> p.toString().endsWith("py")).collect(toList());
			for(Path path : collect)
			{
				try(BufferedReader reader = Files.newBufferedReader(path))
				{
					PYTHON_ENGINE.eval(reader);
				}
			}
		}
		catch(IOException | ScriptException e)
		{
			LOGGER.atSevere().withCause(e).log("Unable to initialize utility scripts: %s",  Constant.QA_QC_SCRIPT_DIRECTORY);
		}
	}

	private void initializeGlobalVariables(CommonPeriodFilter commonPeriodFilter)
	{
		_dssReader = new DssReader(_epptScenarioRun, _waterYearDefinition, _dssCache);
		TitleReader titleReader = new TitleReader(_epptScenarioRun);
		PYTHON_ENGINE.put("dssReader", _dssReader);
		PYTHON_ENGINE.put("titleReader", titleReader);
		PYTHON_ENGINE.put("commonPeriodFilter", commonPeriodFilter);
		PYTHON_ENGINE.put("annualCommonPeriodFilter", (AnnualPeriodFilter) input ->
		{
			Integer year = input.getKey();
			return year >= commonPeriodFilter.getStart().getYear()
					&& year <= commonPeriodFilter.getEnd().getYear();
		});
		setWaterYearType(null);
		setWaterYearIndex(null);
		setWaterYearPeriodRanges(null);
		setPeriodFilter(null);
		setAnnualPeriodFilter(null);
		setComparisonValue(null);
	}

	public void setPeriodFilter(PeriodFilter periodFilter)
	{
		PYTHON_ENGINE.put("periodFilter", periodFilter);
	}

	public Object runScript(String script) throws ScriptException
	{
		return PYTHON_ENGINE.eval(script);
	}

	public void setWaterYearPeriodRanges(List<WaterYearPeriodRange> waterYearPeriodRanges)
	{
		PYTHON_ENGINE.put("waterYearPeriodRanges", waterYearPeriodRanges);
	}

	public void setComparisonValue(Double comparisonValue)
	{
		PYTHON_ENGINE.put("comparisonValue", comparisonValue);
	}

	void setWaterYearType(WaterYearPeriod waterYearType)
	{
		PYTHON_ENGINE.put("waterYearType", waterYearType);
	}

	public void setWaterYearIndex(WaterYearIndex waterYearIndex)
	{
		PYTHON_ENGINE.put("waterYearIndex", waterYearIndex);
	}

	public String getUnits()
	{
		return _dssReader.getUnits();
	}

	public void setAnnualPeriodFilter(AnnualPeriodFilter annualPeriodFilter)
	{
		PYTHON_ENGINE.put("annualPeriodFilter", annualPeriodFilter);
	}
}

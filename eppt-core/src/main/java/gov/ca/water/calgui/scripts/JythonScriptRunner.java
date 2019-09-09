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

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.bo.PeriodFilter;
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
	private final ScriptEngine _engine;
	private final EpptScenarioRun _epptScenarioRun;

	public JythonScriptRunner(EpptScenarioRun epptScenarioRun, CommonPeriodFilter commonPeriodFilter) throws ScriptException
	{
		_epptScenarioRun = epptScenarioRun;
		_engine = new ScriptEngineManager().getEngineByName("python");
		initializeGlobalVariables(commonPeriodFilter);
		initializeScriptDirectory();
	}

	private void initializeScriptDirectory() throws ScriptException
	{
		try(Stream<Path> stream = Files.walk(Constant.QA_QC_SCRIPT_DIRECTORY, 1))
		{
			List<Path> collect = stream.filter(p -> p.toString().endsWith("py")).collect(toList());
			for(Path path : collect)
			{
				try(BufferedReader reader = Files.newBufferedReader(path))
				{
					_engine.eval(reader);
				}
			}
		}
		catch(IOException e)
		{
			ScriptException scriptException = new ScriptException("Unable to read script directory: " + Constant.QA_QC_SCRIPT_DIRECTORY);
			scriptException.initCause(e);
			throw scriptException;
		}
	}

	private void initializeGlobalVariables(CommonPeriodFilter commonPeriodFilter) throws ScriptException
	{
		DssReader dssReader = new DssReader(_epptScenarioRun);
		try
		{
			TitleReader titleReader = new TitleReader(_epptScenarioRun);
			_engine.put("dssReader", dssReader);
			_engine.put("titleReader", titleReader);
			_engine.eval("from gov.ca.water.calgui.scripts import JythonScriptRunner");
			_engine.eval("from gov.ca.water.calgui.scripts.JythonScriptRunner import *");
			_engine.eval("from java.util.stream.Collectors import *");
			_engine.eval("from java.time import Month");
			_engine.put("commonPeriodFilter", commonPeriodFilter);
		}
		catch(EpptInitializationException e)
		{
			ScriptException scriptException = new ScriptException("Unable to create Title Reader");
			scriptException.initCause(e);
			throw scriptException;
		}
	}

	public void setPeriodFilter(PeriodFilter periodFilter)
	{
		_engine.put("periodFilter", periodFilter);
	}

	public Object runScript(String script) throws ScriptException
	{
		return _engine.eval(script);
	}

	public void setWaterYearPeriodRange(WaterYearPeriodRange waterYearPeriodRange)
	{
		_engine.put("waterYearPeriodRange", waterYearPeriodRange);
	}

	public void setComparisonValue(double comparisonValue)
	{
		_engine.put("comparisonValue", comparisonValue);
	}

	public void setWaterYearType(WaterYearPeriod waterYearType)
	{
		_engine.put("waterYearType", waterYearType);
	}

	public void setWaterYearIndex(WaterYearIndex waterYearIndex)
	{
		_engine.put("waterYearIndex", waterYearIndex);
	}
}

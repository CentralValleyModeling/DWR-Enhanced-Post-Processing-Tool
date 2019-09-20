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
	private static final ScriptEngine PYTHON_ENGINE = new ScriptEngineManager().getEngineByName("python");
	private final EpptScenarioRun _epptScenarioRun;
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	static
	{
		try
		{
			initializeScriptDirectory();
		}
		catch(Throwable e)
		{
			LOGGER.atSevere().withCause(e).log("Unable to initialize utility scripts");
		}
	}

	public JythonScriptRunner(EpptScenarioRun epptScenarioRun, CommonPeriodFilter commonPeriodFilter)
	{

		_epptScenarioRun = epptScenarioRun;
		if(PYTHON_ENGINE == null)
		{
			throw new IllegalArgumentException("Unable to find jython engine");
		}
		//			LocalDateTime s1 = LocalDateTime.now();
		initializeGlobalVariables(commonPeriodFilter);
		//			System.out.println("Python script runner instantiation takes" + ChronoUnit.MILLIS.between(s1, LocalDateTime.now()));
	}

	private static void initializeScriptDirectory() throws ScriptException
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
		catch(IOException e)
		{
			throw new IllegalArgumentException("Unable to read script directory: " + Constant.QA_QC_SCRIPT_DIRECTORY, e);
		}
	}

	private void initializeGlobalVariables(CommonPeriodFilter commonPeriodFilter)
	{
		DssReader dssReader = new DssReader(_epptScenarioRun);
		TitleReader titleReader = new TitleReader(_epptScenarioRun);
		PYTHON_ENGINE.put("dssReader", dssReader);
		PYTHON_ENGINE.put("titleReader", titleReader);
		PYTHON_ENGINE.put("commonPeriodFilter", commonPeriodFilter);
		setWaterYearType(null);
		setWaterYearIndex(null);
		setWaterYearPeriodRanges(null);
		setPeriodFilter(null);
		setComparisonValue(null);
	}

	public void setPeriodFilter(PeriodFilter periodFilter)
	{
		PYTHON_ENGINE.put("periodFilter", periodFilter);
	}

	public Object runScript(String script) throws ScriptException
	{
//		initializeScriptDirectory();
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

	public void setWaterYearType(WaterYearPeriod waterYearType)
	{
		PYTHON_ENGINE.put("waterYearType", waterYearType);
	}

	public void setWaterYearIndex(WaterYearIndex waterYearIndex)
	{
		PYTHON_ENGINE.put("waterYearIndex", waterYearIndex);
	}
}

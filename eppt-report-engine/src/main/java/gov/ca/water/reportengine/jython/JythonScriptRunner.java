/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.reportengine.jython;

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
import gov.ca.water.calgui.scripts.DssCache;
import gov.ca.water.calgui.scripts.DssReader;
import gov.ca.water.calgui.scripts.TitleReader;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-05-2019
 */
public class JythonScriptRunner
{
	private final EpptScenarioRun _epptScenarioRun;
	private final CommonPeriodFilter _commonPeriodFilter;
	private final WaterYearDefinition _waterYearDefinition;
	private final DssCache _dssCache;
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	private DssReader _dssReader;
	private PeriodFilter _periodFilter;
	private List<WaterYearPeriodRange> _waterYearPeriodRanges;
	private Double _comparisonValue;
	private WaterYearPeriod _waterYearType;
	private WaterYearIndex _waterYearIndex;
	private AnnualPeriodFilter _annualPeriodFilter;

	public JythonScriptRunner(EpptScenarioRun epptScenarioRun, CommonPeriodFilter commonPeriodFilter,
							  WaterYearDefinition waterYearDefinition, DssCache dssCache)
	{

		_epptScenarioRun = epptScenarioRun;
		_commonPeriodFilter = commonPeriodFilter;
		_waterYearDefinition = waterYearDefinition;
		_dssCache = dssCache;
	}

	private void initializeGlobalVariables(JythonScript jythonScript)
	{
		_dssReader = new DssReader(_epptScenarioRun, _waterYearDefinition, _dssCache);
		TitleReader titleReader = new TitleReader(_epptScenarioRun);
		jythonScript.put("dssReader", _dssReader);
		jythonScript.put("titleReader", titleReader);
		jythonScript.put("commonPeriodFilter", _commonPeriodFilter);
		jythonScript.put("annualCommonPeriodFilter", (AnnualPeriodFilter) input ->
		{
			Integer year = input.getKey();
			return year >= _commonPeriodFilter.getStart().getYear()
					&& year <= _commonPeriodFilter.getEnd().getYear();
		});
		jythonScript.put("periodFilter", _periodFilter);
		jythonScript.put("waterYearPeriodRanges", _waterYearPeriodRanges);
		jythonScript.put("comparisonValue", _comparisonValue);
		jythonScript.put("waterYearType", _waterYearType);
		jythonScript.put("waterYearIndex", _waterYearIndex);
		jythonScript.put("annualPeriodFilter", _annualPeriodFilter);
	}

	public void setPeriodFilter(PeriodFilter periodFilter)
	{
		_periodFilter = periodFilter;
	}

	public Object runScript(JythonScript script) throws ScriptException
	{
		initializeGlobalVariables(script);
		return script.eval();
	}

	public void setWaterYearPeriodRanges(List<WaterYearPeriodRange> waterYearPeriodRanges)
	{
		_waterYearPeriodRanges = waterYearPeriodRanges;
	}

	public void setComparisonValue(Double comparisonValue)
	{
		_comparisonValue = comparisonValue;
	}

	void setWaterYearType(WaterYearPeriod waterYearType)
	{
		_waterYearType = waterYearType;
	}

	public void setWaterYearIndex(WaterYearIndex waterYearIndex)
	{
		_waterYearIndex = waterYearIndex;
	}

	public String getUnits()
	{
		return _dssReader.getUnits();
	}

	public void setAnnualPeriodFilter(AnnualPeriodFilter annualPeriodFilter)
	{
		_annualPeriodFilter = annualPeriodFilter;
	}
}

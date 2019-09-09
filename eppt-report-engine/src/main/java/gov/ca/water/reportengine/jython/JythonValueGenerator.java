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

package gov.ca.water.reportengine.jython;

import java.time.Month;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import javax.script.ScriptException;

import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.bo.PeriodFilter;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.JythonScriptRunner;
import gov.ca.water.reportengine.EpptReportException;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-27-2019
 */
public class JythonValueGenerator
{
	private final PeriodFilter _periodFilter;
	private final EpptScenarioRun _scenarioRun;
	private final String _function;
	private final JythonScriptRunner _scriptRunner;

	public JythonValueGenerator(EpptScenarioRun scenarioRun, String function, CommonPeriodFilter commonPeriodFilter) throws ScriptException
	{
		this(input -> true, scenarioRun, function, commonPeriodFilter);
	}

	public JythonValueGenerator(PeriodFilter periodFilter, EpptScenarioRun base, String function, CommonPeriodFilter commonPeriodFilter)
			throws ScriptException
	{
		_scenarioRun = base;
		_function = JythonScriptBuilder.getInstance().buildFunctionFromTemplate(function);
		_periodFilter = periodFilter;
		_scriptRunner = new JythonScriptRunner(_scenarioRun, commonPeriodFilter);
	}

	public Double generateValue() throws EpptReportException
	{
		try
		{
			Object o = _scriptRunner.runScript(_function);
			return (Double) o;
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error running script: " + _function);
		}
		catch(ClassCastException e)
		{
			throw new EpptReportException("Incorrect return type from function: " + _function +
					" Required: " + Double.class, e);
		}
	}

	public Object generateObjectValue() throws EpptReportException
	{
		try
		{
			return _scriptRunner.runScript(_function);
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error running script: " + _function);
		}
	}

	@SuppressWarnings("unchecked")
	public List<Double> generateValues() throws EpptReportException
	{
		try
		{
			Object o = _scriptRunner.runScript(_function);
			return (List<Double>) o;
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error running script: " + _function);
		}
		catch(ClassCastException e)
		{
			throw new EpptReportException("Incorrect return type from function: " + _function +
					" Required: " + List.class, e);
		}
	}

	@SuppressWarnings("unchecked")
	public NavigableMap<Double, Double> generateExceedanceValues() throws EpptReportException
	{
		try
		{
			Object o = _scriptRunner.runScript(_function);
			return (NavigableMap<Double, Double>) o;
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error running script: " + _function);
		}
		catch(ClassCastException e)
		{
			throw new EpptReportException("Incorrect return type from function: " + _function +
					" Required: " + List.class, e);
		}
	}

	@SuppressWarnings("unchecked")
	public Map<Integer, Double> generateAnnualValues() throws EpptReportException
	{
		try
		{
			Object o = _scriptRunner.runScript(_function);
			return (Map<Integer, Double>) o;
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error running script: " + _function);
		}
		catch(ClassCastException e)
		{
			throw new EpptReportException("Incorrect return type from function: " + _function +
					" Required: " + List.class, e);
		}
	}

	@SuppressWarnings("unchecked")
	public Map<Month, Double> generateMonthlyValues() throws EpptReportException
	{
		try
		{
			Object o = _scriptRunner.runScript(_function);
			return (Map<Month, Double>) o;
		}
		catch(ScriptException e)
		{
			throw new EpptReportException("Error running script: " + _function);
		}
		catch(ClassCastException e)
		{
			throw new EpptReportException("Incorrect return type from function: " + _function +
					" Required: ", e);
		}
	}
}
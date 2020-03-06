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

package gov.ca.water.calgui.busservice.impl;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.OptionalDouble;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.CompiledScript;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import gov.ca.water.calgui.constant.Constant;
import org.python.jsr223.PyScriptEngine;

import rma.util.lookup.Lookup;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-23-2019
 */
public final class ScriptedEpptStatistics implements EpptStatistic
{
	private static final Logger LOGGER = Logger.getLogger(ScriptedEpptStatistics.class.getName());
	private static List<EpptStatistic> scriptedStatistics = new ArrayList<>();
	private final ScriptEngine _scriptEngine = new ScriptEngineManager(getClass().getClassLoader()).getEngineByName("python");
	private final Path _jythonFilePath;
	private final String _name;
	private CompiledScript _compile;

	private ScriptedEpptStatistics(Path jythonFilePath)
	{
		_jythonFilePath = jythonFilePath;
		_name = loadStatisticName();
		setupScriptEngine();
	}

	public static List<EpptStatistic> getTrendStatistics()
	{
		return scriptedStatistics;
	}

	public static void createScriptedStatistics()
	{
		scriptedStatistics.clear();
		Path jython = Paths.get(Constant.TREND_REPORTING_DIR).resolve("jython");
		try(Stream<Path> stream = Files.walk(jython, 5))
		{
			scriptedStatistics.addAll(stream.filter(p -> p.toFile().isFile()).filter(p -> p.toString().endsWith("py"))
											.map(ScriptedEpptStatistics::new)
											.collect(toList()));
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to load Statistics for Trend Reporting dashboard", e);
		}
		scriptedStatistics.addAll(Lookup.getDefault().lookupAll(EpptStatistic.class));
	}

	private void setupScriptEngine()
	{
		Path jython = Paths.get(Constant.CONFIG_DIR).resolve("jython");
		try(Stream<Path> stream = Files.walk(jython, 5))
		{
			PyScriptEngine pyScriptEngine = (PyScriptEngine) _scriptEngine;
			List<Path> py = stream.filter(p -> p.toFile().isFile()).filter(p -> p.toString().endsWith("py")).collect(Collectors.toList());
			for(Path p : py)
			{
				try(BufferedReader reader = Files.newBufferedReader(p))
				{
					pyScriptEngine.eval(reader);
				}
			}
			try(BufferedReader bufferedReader = Files.newBufferedReader(_jythonFilePath))
			{
				_scriptEngine.eval(bufferedReader);
			}
			_compile = pyScriptEngine.compile("calculate(data)");
		}
		catch(IOException | ScriptException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to load helper scripts from: " + jython, e);
		}
	}

	@Override
	public String getName()
	{
		return _name;
	}

	private Object runScript(List<Double> data) throws ScriptException
	{
		_scriptEngine.put("data", data);
		return _compile.eval();
	}

	private String loadStatisticName()
	{
		String retval = _jythonFilePath.getFileName().toString();

		try(BufferedReader bufferedReader = Files.newBufferedReader(_jythonFilePath))
		{
			_scriptEngine.eval(bufferedReader);
			retval = Objects.toString(_scriptEngine.eval("getName()"));
		}
		catch(IOException | ScriptException e)
		{
			LOGGER.log(Level.SEVERE, "Error getting name from Jython script: " + _jythonFilePath + " ensure method getName() is defined", e);
		}
		return retval;
	}

	@Override
	public Double calculateYearly(List<Double> data)
	{
		Double retval = Double.NaN;
		try
		{
			Object obj = runScript(data);
			if(obj instanceof OptionalDouble)
			{
				OptionalDouble opt = ((OptionalDouble) obj);
				if(opt.isPresent())
				{
					retval = opt.getAsDouble();
				}
			}
			else if(obj instanceof Double)
			{
				retval = (Double) obj;
			}
		}
		catch(ClassCastException e)
		{
			LOGGER.log(Level.SEVERE, "Error computing statistic " + getName() + " from Jython script: " + _jythonFilePath +
					" ensure method calculate(Map<LocalDateTime, Double> data) returns a Map<? extends Month, ? extends Double>", e);
		}
		catch(ScriptException e)
		{
			LOGGER.log(Level.SEVERE, "Error computing statistic " + getName() + " from Jython script: " + _jythonFilePath +
					" ensure method calculate(Map<LocalDateTime, Double> data) is defined", e);
		}
		return retval;
	}

	@Override
	public String toString()
	{
		return getName();
	}
}

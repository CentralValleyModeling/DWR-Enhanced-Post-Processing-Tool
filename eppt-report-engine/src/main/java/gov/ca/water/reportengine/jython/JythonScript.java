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
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.script.CompiledScript;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import gov.ca.water.calgui.constant.Constant;
import org.python.jsr223.PyScriptEngine;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-29-2020
 */
public class JythonScript
{
	private final ScriptEngine _scriptEngine = new ScriptEngineManager(getClass().getClassLoader()).getEngineByName("python");
	private final String _name;
	private final List<String> _arguments;
	private final String _scriptTemplate;
	private CompiledScript _compile;

	JythonScript(String name, List<String> arguments, String scriptTemplate) throws IOException, ScriptException
	{
		_name = name;
		_arguments = arguments;
		_scriptTemplate = scriptTemplate;
		setupScriptEngine();
	}

	public String getName()
	{
		return _name;
	}

	public List<String> getArguments()
	{
		return _arguments;
	}
	private void setupScriptEngine() throws IOException, ScriptException
	{
		Path jython = Paths.get(Constant.CONFIG_DIR).resolve("jython");
		PyScriptEngine pyScriptEngine = (PyScriptEngine) _scriptEngine;
		try(Stream<Path> stream = Files.walk(jython, 5))
		{
			List<Path> py = stream.filter(p -> p.toFile().isFile()).filter(p -> p.toString().endsWith("py")).collect(Collectors.toList());
			for(Path p : py)
			{
				try(BufferedReader reader = Files.newBufferedReader(p))
				{
					pyScriptEngine.eval(reader);
				}
			}
		}
		try(Stream<Path> qaStream = Files.walk(Constant.QA_QC_SCRIPT_DIRECTORY, 1))
		{
			List<Path> collect = qaStream.filter(p -> p.toString().endsWith("py")).collect(toList());
			for(Path path : collect)
			{
				try(BufferedReader reader = Files.newBufferedReader(path))
				{
					pyScriptEngine.eval(reader);
				}
			}
		}
		_compile = pyScriptEngine.compile(_scriptTemplate.replace("${", "").replace("}", ""));
	}

	public void put(String key, Object value)
	{
		_scriptEngine.put(key, value);
	}

	public Object eval() throws ScriptException
	{
		return _compile.eval();
	}

	@Override
	public String toString()
	{
		return getName();
	}
}

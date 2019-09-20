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

import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.impl.FilePredicates;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-09-2019
 */
public final class JythonScriptBuilder
{
	private static final Pattern CSV_PATTERN = Pattern.compile(",");
	private static final Pattern OPEN_PAREN_PATTERN = Pattern.compile("\\(");
	private static final Pattern CLOSED_PAREN_PATTERN = Pattern.compile("\\)");
	private static final int FUNCTION_NAME_INDEX = 1;
	private static final int FUNCTION_TEMPLATE_INDEX = 2;
	private static JythonScriptBuilder instance;

	private final List<Script> _scripts;

	private JythonScriptBuilder() throws EpptInitializationException
	{
		_scripts = readJythonScripts();
	}

	private List<Script> readJythonScripts() throws EpptInitializationException
	{
		try(Stream<String> stream = Files.lines(Constant.FUNCTIONS_PATH))
		{
			return stream.filter(FilePredicates.commentFilter()).map(this::buildScriptFromLine).filter(Objects::nonNull).collect(toList());
		}
		catch(IOException | RuntimeException e)
		{
			throw new EpptInitializationException("Error reading " + Constant.FUNCTIONS_PATH, e);
		}
	}

	private Script buildScriptFromLine(String line)
	{
		Script retval = null;
		String[] split = CSV_PATTERN.split(line);
		replaceCommas(split);
		if(FUNCTION_TEMPLATE_INDEX < split.length)
		{
			String fullFunctionName = split[FUNCTION_NAME_INDEX];
			String[] nameSplit = OPEN_PAREN_PATTERN.split(fullFunctionName);
			List<String> arguments = new ArrayList<>();
			if(nameSplit.length > 1)
			{
				String[] argumentsListed = CLOSED_PAREN_PATTERN.split(nameSplit[1]);
				Collections.addAll(arguments, CSV_PATTERN.split(argumentsListed[0]));
			}
			retval = new Script(nameSplit[0], arguments, split[FUNCTION_TEMPLATE_INDEX]);
		}
		return retval;
	}

	private void replaceCommas(String[] input)
	{
		for(int i = 0; i < input.length; i++)
		{
			input[i] = input[i].replace("%2C", ",");
		}
	}

	public static void createInstance() throws EpptInitializationException
	{
		if(instance == null)
		{
			instance = new JythonScriptBuilder();
		}
		else
		{
			throw new EpptInitializationException("Script Builder already initialized");
		}
	}

	public static JythonScriptBuilder getInstance()
	{
		return instance;
	}

	public String buildFunctionFromTemplate(String functionReference)
	{
//		try
//		{
//			List<Script> scripts = readJythonScripts();
//			_scripts.clear();
//			_scripts.addAll(scripts);
//		}
//		catch(EpptInitializationException e)
//		{
//			e.printStackTrace();
//		}
		return getMatchingScriptTemplate(functionReference)
				.orElseThrow(() -> new IllegalArgumentException("Illegal script: " + functionReference));
	}

	private Optional<String> getMatchingScriptTemplate(String functionReference)
	{
		String[] nameSplit = OPEN_PAREN_PATTERN.split(functionReference);
		return _scripts.stream().filter(f -> f._name.equals(nameSplit[0]))
					   .map(f -> buildFunctionFromTemplate(f, functionReference))
					   .findAny();
	}

	private String buildFunctionFromTemplate(Script script, String functionReference)
	{
		String retval = script._scriptTemplate;
		String[] split = OPEN_PAREN_PATTERN.split(functionReference);
		if(split.length > 1)
		{
			String[] arguments = CLOSED_PAREN_PATTERN.split(split[1]);
			String argumentsList = arguments[0];
			String[] arg = CSV_PATTERN.split(argumentsList);
			if(arg.length != script._arguments.size())
			{
				throw new IllegalArgumentException(
						"Function: " + script._name + " Arguments length differs from template length. Arguments: " + Arrays.toString(
								arg) + " Template: " + script._arguments);
			}
			for(int i = 0; i < arg.length; i++)
			{
				String s = script._arguments.get(i);
				retval = retval.replace(s.trim(), arg[i].trim());
			}
		}
		return retval;
	}

	public Optional<String> getScript(String reference)
	{
		return getMatchingScriptTemplate(reference);
	}

	private final class Script
	{
		private final String _name;
		private final List<String> _arguments;
		private final String _scriptTemplate;

		private Script(String name, List<String> arguments, String scriptTemplate)
		{
			_name = name;
			_arguments = arguments;
			_scriptTemplate = scriptTemplate;
		}
	}
}

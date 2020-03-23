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
import java.time.Month;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import javax.script.ScriptException;

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
	private static final Logger LOGGER = Logger.getLogger(JythonScriptBuilder.class.getName());
	private static final Pattern CSV_PATTERN = Pattern.compile(",");
	private static final Pattern OPEN_PAREN_PATTERN = Pattern.compile("\\(");
	private static final Pattern CLOSED_PAREN_PATTERN = Pattern.compile("\\)");
	private static final int FUNCTION_NAME_INDEX = 1;
	private static final int FUNCTION_TEMPLATE_INDEX = 2;
	private static JythonScriptBuilder instance;

	private final List<JythonScript> _scripts;

	private JythonScriptBuilder() throws EpptInitializationException
	{
		_scripts = readJythonScripts();
	}

	private List<JythonScript> readJythonScripts() throws EpptInitializationException
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

	private JythonScript buildScriptFromLine(String line)
	{

		JythonScript retval = null;
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
			try
			{
				retval = new JythonScript(nameSplit[0], arguments, split[FUNCTION_TEMPLATE_INDEX]);
			}
			catch(IOException | ScriptException ex)
			{
				throw new IllegalStateException("Unable to load script: " + nameSplit[0], ex);
			}
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
		instance = new JythonScriptBuilder();
	}

	public static JythonScriptBuilder getInstance()
	{
		return instance;
	}

	public JythonScript buildFunctionFromTemplate(String functionReference)
	{
		return getMatchingScriptTemplate(functionReference)
				.orElseThrow(
						() -> new IllegalArgumentException("No matching script defined in " + Constant.FUNCTIONS_PATH + ": " + functionReference));
	}

	private Optional<JythonScript> getMatchingScriptTemplate(String functionReference)
	{
		String[] nameSplit = OPEN_PAREN_PATTERN.split(functionReference);
		Optional<JythonScript> retval = _scripts.stream().filter(f -> f.getName().equals(nameSplit[0]))
												.findAny();
		retval.ifPresent(s -> buildFunctionFromTemplate(s, functionReference));
		return retval;
	}

	private void buildFunctionFromTemplate(JythonScript script, String functionReference)
	{
		String[] split = OPEN_PAREN_PATTERN.split(functionReference);
		if(split.length > 1)
		{
			String[] arguments = CLOSED_PAREN_PATTERN.split(split[1]);
			if(arguments.length > 0)
			{
				String argumentsList = arguments[0];
				String[] arg = CSV_PATTERN.split(argumentsList);
				if(arg.length != script.getArguments().size())
				{
					throw new IllegalArgumentException(
							"Function: " + script.getName() + " Arguments length differs from template length. Arguments: " + Arrays.toString(
									arg) + " Template: " + script.getArguments());
				}
				for(int i = 0; i < arg.length; i++)
				{
					String s = script.getArguments().get(i);
					String key = s.trim().replace("${", "").replace("}", "");
					String valueString = arg[i].trim().replace("${", "").replace("}", "");
					Object value = valueString;
					try
					{
						value = Integer.parseInt(valueString);
					}
					catch(RuntimeException e)
					{
						LOGGER.log(Level.FINER, "Not an integer", e);
						try
						{
							value = Double.parseDouble(valueString);
						}
						catch(RuntimeException ex)
						{
							LOGGER.log(Level.FINER, "Not a double", ex);
							try
							{
								value = Month.valueOf(valueString.replace("Month.", ""));
							}
							catch(RuntimeException ex2)
							{
								LOGGER.log(Level.FINER, "Not a double", ex2);
							}
						}
					}
					script.put(key, value);
				}
			}
			else
			{
				LOGGER.log(Level.WARNING, "Missing closed parenthesis in function: {0}", script);
			}
		}
		else
		{
			LOGGER.log(Level.WARNING, "Missing open parenthesis in function: {0}", script);
		}
	}

	public Optional<JythonScript> getScript(String reference)
	{
		return getMatchingScriptTemplate(reference);
	}

}

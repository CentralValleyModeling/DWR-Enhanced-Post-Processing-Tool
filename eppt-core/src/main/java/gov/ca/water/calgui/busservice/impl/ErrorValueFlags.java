/*
 * Copyright (c) 2020
 * United States Army Corps of Engineers - Hydrologic Engineering Center (USACE/HEC)
 * All Rights Reserved.  USACE PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from HEC
 */

package gov.ca.water.calgui.busservice.impl;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.impl.FilePredicates;

import static java.util.stream.Collectors.toSet;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 06-08-2020
 */
public class ErrorValueFlags
{

	private static ErrorValueFlags instance;
	private final Path _errorValueFlagPath;
	private final Set<Double> _errorValueFlags = new HashSet<>();

	private ErrorValueFlags(Path errorValueFlagPath) throws EpptInitializationException
	{
		_errorValueFlagPath = errorValueFlagPath;
		_errorValueFlags.addAll(read());
	}

	private Set<? extends Double> read() throws EpptInitializationException
	{
		try
		{
			return Files.readAllLines(_errorValueFlagPath)
						.stream()
						.filter(FilePredicates.commentFilter())
						.flatMap(s -> Arrays.stream(s.split(",")))
						.map(Double::valueOf)
						.collect(toSet());
		}
		catch(IOException | NumberFormatException e)
		{
			throw new EpptInitializationException("Unable to read error value flag file: " + _errorValueFlagPath, e);
		}
	}

	public static void initializeErrorFlags() throws EpptInitializationException
	{
		instance = new ErrorValueFlags(Paths.get(Constant.ERROR_VALUE_FLAGS));
	}

	public static boolean isErrorValue(Double value)
	{
		if(Constant.isValidValue(value))
		{
			value = BigDecimal.valueOf(value).setScale(6, RoundingMode.HALF_UP).doubleValue();
		}
		Double funcVal = value;
		return instance._errorValueFlags.stream().anyMatch(d -> Objects.equals(d, funcVal));
	}
}

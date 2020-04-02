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

package gov.ca.water.calgui.busservice.impl;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.constant.Constant;

import static java.util.stream.Collectors.toList;

public class WaterYearNameLookup
{

	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private static final Path WATER_YEAR_TYPE_NAMES = Paths.get(Constant.WY_TYPES_NAME_LOOKUP);
	private final Map<String, List<String>> _colHeadersToValues;

	public WaterYearNameLookup() throws EpptInitializationException
	{
		LOGGER.at(Level.FINE).log("Reading Water Year Type Name Lookup: %s", WATER_YEAR_TYPE_NAMES);
		String line = "";
		String csvSplitBy = ",";

		List<String> colHeaders = new ArrayList<>();

		Map<String, List<String>> columnsToValues = new HashMap<>();

		try(BufferedReader br = Files.newBufferedReader(WATER_YEAR_TYPE_NAMES))
		{

			boolean columnHeadersHaveNotBeenDefined = true;
			while((line = br.readLine()) != null)
			{
				// use comma as separator
				String[] row = line.split(csvSplitBy);
				//skip comments
				if(row.length > 0)
				{
					String firstString = row[0];
					String trimmedString = firstString.trim();
					if(trimmedString.length() > 0)
					{


						char firstChar = trimmedString.charAt(0);
						if(firstChar == '#' || firstChar == '!')
						{
							continue;
						}
					}
					if(columnHeadersHaveNotBeenDefined)
					{
						columnHeadersHaveNotBeenDefined = false;

						for(final String s : row)
						{
							//skip the first column
							colHeaders.add(s);
							columnsToValues.put(s, new ArrayList<>());
						}
					}
					else
					{
						//for all other rows other than column headers
						for(int i = 1; i < colHeaders.size(); i++)
						{
							//add the value from each column into the dictionary
							//basically taking a horizontal row and putting it into a dictionary that goes vertacally
							String value = "Undefined";
							if(row.length > i)
							{
								value = row[i];
							}
							if(Objects.equals(value, ""))
							{
								value = "Undefined";
							}
							columnsToValues.get(colHeaders.get(i)).add(value);
						}
					}
				}
			}

		}
		catch(IOException e)
		{
			throw new EpptInitializationException("Error reading the water year type name lookup csv file: " + WATER_YEAR_TYPE_NAMES, e);
		}
		_colHeadersToValues = columnsToValues;
	}

	String getWaterYearType(int waterYearTypeNum, String columnHeader)
	{
		String retval = "Undefined";
		if(_colHeadersToValues.containsKey(columnHeader))
		{
			List<String> values = _colHeadersToValues.get(columnHeader);
			if(values.size() > waterYearTypeNum)
			{
				retval = values.get(waterYearTypeNum);
			}
		}
		return retval;
	}

	List<WaterYearPeriod> getSortedWaterYearPeriods(String waterYearIndexColHeader)
	{
		return _colHeadersToValues.getOrDefault(waterYearIndexColHeader, new ArrayList<>())
								  .stream()
								  .filter(Objects::nonNull)
								  .filter(s -> !s.isEmpty())
								  .filter(s -> !"Undefined".equalsIgnoreCase(s))
								  .map(WaterYearPeriod::new)
								  .collect(toList());

	}
}

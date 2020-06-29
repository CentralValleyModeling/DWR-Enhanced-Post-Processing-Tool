/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.calgui.busservice.impl;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.stream.Stream;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.impl.FilePredicates;

import static java.util.stream.Collectors.toList;

public class WaterYearNameLookup
{

	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private static final Path WATER_YEAR_TYPE_NAMES = Paths.get(Constant.WY_TYPES_NAME_LOOKUP);
	private final Map<Integer, List<String>> _colHeadersToValues;

	public WaterYearNameLookup() throws EpptInitializationException
	{
		LOGGER.at(Level.FINE).log("Reading Water Year Type Name Lookup: %s", WATER_YEAR_TYPE_NAMES);
		String csvSplitBy = ",";
		_colHeadersToValues = read(csvSplitBy);
	}

	private Map<Integer, List<String>> read(String csvSplitBy) throws EpptInitializationException
	{
		List<Integer> colHeaders = new ArrayList<>();
		Map<Integer, List<String>> columnsToValues = new HashMap<>();
		List<String> lines = readFile();
		boolean columnHeadersHaveNotBeenDefined = true;
		for(String line : lines)
		{
			// use comma as separator
			String[] row = line.split(csvSplitBy);
			if(columnHeadersHaveNotBeenDefined)
			{
				columnHeadersHaveNotBeenDefined = false;
				readColumnHeaders(colHeaders, columnsToValues, row);
			}
			else
			{
				//for all other rows other than column headers
				for(int i = 1; i <= colHeaders.size(); i++)
				{
					//add the value from each column into the dictionary
					//basically taking a horizontal row and putting it into a dictionary that goes vertically
					String value = "Undefined";
					if(row.length > i)
					{
						value = row[i];
					}
					if(Objects.equals(value, ""))
					{
						value = "Undefined";
					}
					columnsToValues.get(colHeaders.get(i - 1)).add(value);
				}
			}
		}

		return columnsToValues;
	}

	private List<String> readFile() throws EpptInitializationException
	{
		List<String> collect;
		try(Stream<String> lines = Files.lines(WATER_YEAR_TYPE_NAMES))
		{
			collect = lines.filter(FilePredicates.commentFilter()).map(String::trim).filter(s -> !s.isEmpty()).collect(toList());
		}
		catch(IOException e)
		{
			throw new EpptInitializationException("Error reading the water year type name lookup csv file: " + WATER_YEAR_TYPE_NAMES, e);
		}
		return collect;
	}

	private void readColumnHeaders(List<Integer> colHeaders, Map<Integer, List<String>> columnsToValues, String[] row)
	{
		//skip the first column
		row = Arrays.copyOfRange(row, 1, row.length);
		for(final String s : row)
		{
			colHeaders.add(Integer.parseInt(s));
			columnsToValues.put(Integer.parseInt(s), new ArrayList<>());
		}
	}

	String getWaterYearType(int waterYearTypeNum, int columnHeader)
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

	List<WaterYearPeriod> getSortedWaterYearPeriods(int waterYearIndexId)
	{
		return _colHeadersToValues.getOrDefault(waterYearIndexId, new ArrayList<>())
								  .stream()
								  .filter(Objects::nonNull)
								  .filter(s -> !s.isEmpty())
								  .filter(s -> !"Undefined".equalsIgnoreCase(s))
								  .distinct()
								  .map(WaterYearPeriod::new)
								  .collect(toList());

	}
}

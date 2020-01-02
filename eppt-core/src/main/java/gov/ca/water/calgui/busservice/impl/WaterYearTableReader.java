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
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.impl.FilePredicates;

public class WaterYearTableReader
{
	private static final Path WATER_YEAR_TYPE_NAMES = Paths.get(Constant.WY_TYPES_NAME_LOOKUP);
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private static final Pattern WYTPES_TABLE_SPLIT = Pattern.compile("\\s+");

	private final Path _waterYearTypeTable;
	private WaterYearNameLookup _waterYearNameLookupTable;


	public WaterYearTableReader(Path waterYearTypeTable)
	{
		_waterYearTypeTable = waterYearTypeTable;
	}

	public List<WaterYearIndex> read() throws EpptInitializationException
	{
		readNameLookupTable();
		LOGGER.at(Level.FINE).log("Reading Water Year Type Table: %s", _waterYearTypeTable);

		List<WaterYearIndex> waterYearIndices = new ArrayList<>();
		try(Stream<String> lines = Files.lines(_waterYearTypeTable))
		{
			List<String[]> collect = lines.filter(l -> !l.isEmpty())
										  .filter(FilePredicates.commentFilter())
										  .map(WYTPES_TABLE_SPLIT::split)
										  .filter(l -> l.length > 1)
										  .collect(Collectors.toList());
			if(!collect.isEmpty())
			{
				String[] headers = collect.get(0);
				for(int i = 1; i < headers.length; i++)
				{
					List<WaterYearType> waterYearTypes = new ArrayList<>();
					for(int j = 1; j < collect.size(); j++)
					{
						String[] row = collect.get(j);
						if(i < row.length)
						{
							int waterYearCode = Integer.parseInt(row[i]);
							String waterYearType = _waterYearNameLookupTable.getWaterYearType(waterYearCode, headers[i]);
							waterYearTypes.add(new WaterYearType(Integer.parseInt(row[0]), new WaterYearPeriod(waterYearType)));
						}
					}
					List<WaterYearPeriod> waterYearPeriods = _waterYearNameLookupTable.getSortedWaterYearPeriods(headers[i]);
					waterYearIndices.add(new WaterYearIndex(headers[i], waterYearTypes, waterYearPeriods));
				}
			}
		}
		catch(IOException e)
		{
			throw new EpptInitializationException("Error reading the table file: " + _waterYearTypeTable.toAbsolutePath(), e);
		}
		return waterYearIndices;
	}

	private void readNameLookupTable() throws EpptInitializationException
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
			_waterYearNameLookupTable = new WaterYearNameLookup(columnsToValues);

		}
		catch(IOException e)
		{
			throw new EpptInitializationException("Error reading the water year type name lookup csv file: " + WATER_YEAR_TYPE_NAMES, e);
		}
	}
}

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

package gov.ca.water.reportengine.reportreaders;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;

import com.google.common.flogger.FluentLogger;
import gov.ca.water.reportengine.EpptReportException;
import gov.ca.water.reportengine.executivereport.ExecutiveReportException;
import org.python.antlr.base.mod;

public class WaterYearTableReader
{
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();

	private final Path _waterYearTypeTable;
	private final Path _waterYearNameLookup;
	private String[] _columnHeaders;
	private WaterYearNameLookup _waterYearNameLookupTable;


	public WaterYearTableReader(Path waterYearTypeTable, Path waterYearNameLookup)
	{
		_waterYearTypeTable = waterYearTypeTable;
		_waterYearNameLookup = waterYearNameLookup;
	}

	public List<WaterYearType> read() throws EpptReportException
	{
		readNameLookupTable();

		String line = "";
		String whitespaceSplit = "\\s+";
		List<WaterYearType> retval = new ArrayList<>();
		LOGGER.at(Level.INFO).log("Reading Water Year Type Table: %s", _waterYearTypeTable);
		try(BufferedReader br = Files.newBufferedReader(_waterYearTypeTable))
		{
			String[] previousLine = null;
			boolean columnHeadersHaveNotBeenProcessed = true;
			while((line = br.readLine()) != null)
			{
				if(line.startsWith("!") || line.startsWith("#"))
				{
					continue;
				}
				String[] row = line.split(whitespaceSplit);

				//skip comments
				if(row.length > 0)
				{
					String firstString = row[0];
					String trimmedString = firstString.trim();
					//if there is no model id then we continue the for loop
					if(trimmedString.length() > 0)
					{
						char firstChar = trimmedString.charAt(0);

						if(firstChar == '#' || firstChar == '!')
						{
							continue;
						}

						if(isInteger(trimmedString, 10))
						{
							if(columnHeadersHaveNotBeenProcessed)
							{
								//go back one and process the column headers
								columnHeadersHaveNotBeenProcessed = false;
								_columnHeaders = previousLine;

							}
							if(_columnHeaders != null)
							{
								WaterYearType waterYearType = createWaterYearType(_columnHeaders, row);
								if(waterYearType != null)
								{
									retval.add(waterYearType);
								}
							}
						}

						previousLine = row;
					}
				}
			}
		}
		catch(IOException e)
		{
			throw new EpptReportException("Error reading the csv file: ", e);
		}
		return retval;
	}

	private void readNameLookupTable() throws ExecutiveReportException
	{
		LOGGER.at(Level.INFO).log("Reading Water Year Type Name Lookup: %s", _waterYearNameLookup);
		String line = "";
		String csvSplitBy = ",";

		List<String> colHeaders = new ArrayList<>();

		Map<String, List<String>> columnsToValues = new HashMap<>();

		try(BufferedReader br = Files.newBufferedReader(_waterYearNameLookup))
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
			throw new ExecutiveReportException("Error reading the water year type name lookup csv file: " + _waterYearNameLookup, e);
		}
	}

	private WaterYearType createWaterYearType(String[] columnHeaders, String[] values) throws EpptReportException
	{
		//we assume that the year is always the first column, but we don't know the order after that
		WaterYearType retval = null;
		if(values.length > 2)
		{

			try
			{
				int[] vals = new int[values.length];
				for(int i = 0; i < values.length; i++)
				{
					vals[i] = Integer.parseInt(values[i]);
				}
				//get the actual string water year type from the int value in the second column
				String secondColumn = columnHeaders[1];
				int secondValue = vals[1];
				String waterYearType = _waterYearNameLookupTable.getWaterYearType(secondValue, secondColumn);
				retval = new WaterYearType(columnHeaders, vals, waterYearType);

			}
			catch(NumberFormatException ex)
			{
				throw new EpptReportException("Unable to parse the water year type table. Could not convert to integer: " + values[0], ex);
			}
		}
		return retval;
	}


	private boolean isInteger(String s, int radix)
	{
		if(s.isEmpty())
		{
			return false;
		}
		for(int i = 0; i < s.length(); i++)
		{
			if(i == 0 && s.charAt(i) == '-')
			{
				if(s.length() == 1)
				{
					return false;
				}
				else
				{
					continue;
				}
			}
			if(Character.digit(s.charAt(i), radix) < 0)
			{
				return false;
			}
		}
		return true;
	}

}

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
import java.util.Optional;
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
	private static final FluentLogger LOGGER = FluentLogger.forEnclosingClass();
	private static final Pattern WYTPES_TABLE_SPLIT = Pattern.compile("\\s+");

	private final Path _waterYearTypeTable;
	private WaterYearNameLookup _waterYearNameLookupTable;


	public WaterYearTableReader(Path lookupDirectory)
	{
		_waterYearTypeTable = lookupDirectory.resolve(Constant.WY_TYPES_TABLE);
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
					String header = headers[i];
					String waterYearIndexName = WaterYearIndexAliasReader.getInstance().getAliases().stream()
																		 .filter(s -> s.isAliasFor(header))
																		 .findAny()
																		 .map(WaterYearIndexAliasReader.WaterYearIndexAlias::getAlias)
																		 .orElse(header);
					for(int j = 1; j < collect.size(); j++)
					{
						String[] row = collect.get(j);
						if(i < row.length)
						{
							int waterYearCode = Integer.parseInt(row[i]);
							String waterYearType = _waterYearNameLookupTable.getWaterYearType(waterYearCode, waterYearIndexName);
							waterYearTypes.add(new WaterYearType(Integer.parseInt(row[0]), new WaterYearPeriod(waterYearType)));
						}
					}
					List<WaterYearPeriod> waterYearPeriods = _waterYearNameLookupTable.getSortedWaterYearPeriods(waterYearIndexName);
					waterYearIndices.add(new WaterYearIndex(header, waterYearTypes, waterYearPeriods));
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
		_waterYearNameLookupTable = new WaterYearNameLookup();
	}
}

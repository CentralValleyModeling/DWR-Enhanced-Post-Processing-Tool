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
import java.time.Month;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.techservice.impl.FilePredicates;

import hec.heclib.dss.DSSPathname;

import static gov.ca.water.calgui.constant.Constant.CSV_PATTERN_DELIMITER;
import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 06-16-2020
 */
public class WaterYearIndexModelReader
{
	private static final int INDEX_ID_COL = 0;
	private static final int DSS_A_PART_COL = 1;
	private static final int DSS_B_PART_COL = 2;
	private static final int DSS_C_PART_COL = 3;
	private static final int MONTH_COLL = 4;
	private static final int TABLE_HEADER_COL = 5;
	private final Map<Integer, DSSPathname> _pathnames = new HashMap<>();
	private final Map<String, Integer> _tableFileHeaders = new HashMap<>();
	private final Map<Integer, Month> _months = new HashMap<>();

	public WaterYearIndexModelReader(EpptScenarioRun epptScenarioRun) throws EpptInitializationException
	{
		Path modelWaterYearIndexFile = EpptPreferences.getLastProjectConfiguration().getParent().resolve(epptScenarioRun.getName()).resolve(Constant.MODEL_WATER_YEAR_INDEX_FILE);
		if(!modelWaterYearIndexFile.toFile().exists())
		{
			modelWaterYearIndexFile = Paths.get(Constant.CONFIG_DIR).resolve(epptScenarioRun.getModel().toString()).resolve(Constant.MODEL_WATER_YEAR_INDEX_FILE);
		}
		try(Stream<String> stream = Files.lines(modelWaterYearIndexFile))
		{
			List<String[]> lines = stream.filter(FilePredicates.commentFilter())
										 .map(String::trim)
										 .filter(s -> !s.isEmpty())
										 .map(CSV_PATTERN_DELIMITER::split)
										 .filter(s -> s.length >= 6)
										 .collect(toList());
			for(String[] line : lines)
			{
				int indexId = Integer.parseInt(line[INDEX_ID_COL]);
				String aPart = line[DSS_A_PART_COL];
				String bPart = line[DSS_B_PART_COL];
				String cPart = line[DSS_C_PART_COL];
				String monthString = line[MONTH_COLL].toUpperCase().trim();
				Month month = null;
				if(!monthString.isEmpty())
				{
					month = Month.valueOf(monthString.toUpperCase());
				}
				String tableHeader = line[TABLE_HEADER_COL];
				DSSPathname dssPathname = new DSSPathname();
				dssPathname.setAPart(aPart);
				dssPathname.setBPart(bPart);
				dssPathname.setCPart(cPart);
				if(month != null)
				{
					_pathnames.put(indexId, dssPathname);
					_months.put(indexId, month);
				}
				_tableFileHeaders.put(tableHeader.toLowerCase(), indexId);
			}
		}
		catch(IOException | RuntimeException e)
		{
			throw new EpptInitializationException(
					"Error reading water year index model file: " + modelWaterYearIndexFile + " \nUnable to determine water year types for scenario: " + epptScenarioRun, e);
		}
	}

	public Optional<Integer> getIndexIdForTableHeader(String header)
	{
		return Optional.ofNullable(_tableFileHeaders.get(header.toLowerCase()));
	}

	public Month getMonth(Integer waterYearIndexId)
	{
		return _months.get(waterYearIndexId);
	}

	public Map<Integer, DSSPathname> getWaterYearIndexIds()
	{
		return _pathnames;
	}
}

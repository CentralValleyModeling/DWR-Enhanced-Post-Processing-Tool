/*
 * Copyright (c) 2020
 * United States Army Corps of Engineers - Hydrologic Engineering Center (USACE/HEC)
 * All Rights Reserved.  USACE PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from HEC
 */

package gov.ca.water.calgui.busservice.impl;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IFileSystemSvc;
import gov.ca.water.calgui.techservice.impl.FilePredicates;
import gov.ca.water.calgui.techservice.impl.FileSystemSvcImpl;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-14-2020
 */
public class WaterYearPeriodReader
{
	private static final Logger LOGGER = Logger.getLogger(WaterYearPeriodReader.class.getName());
	private static final int INDEX_COLUMN = 0;
	private static final int DISPLAY_NAME_COLUMN = 1;
	private static final int START_YEAR_COLUMN = 2;
	private static final int END_YEAR_COLUMN = 3;

	private final List<WaterYearPeriodDefinition> _waterYearPeriodDefinitions = new ArrayList<>();
	private static WaterYearPeriodReader instance;

	public static WaterYearPeriodReader getInstance()
	{
		return instance;
	}

	public static void createInstance()
	{
		instance = new WaterYearPeriodReader();
		instance.read();
	}

	public List<WaterYearPeriodDefinition> getWaterYearPeriodDefinitions()
	{
		return new ArrayList<>(_waterYearPeriodDefinitions);
	}

	private void read()
	{
		_waterYearPeriodDefinitions.clear();
		Path file = Paths.get(Constant.CONFIG_DIR).resolve("AnnualPeriodFilters.csv");
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		try
		{
			List<String> rows = fileSystemSvc.getFileData(file, true, FilePredicates.commentFilter());
			for(String row : rows)
			{
				String[] list = row.split(Constant.DELIMITER);
				_waterYearPeriodDefinitions.add(new WaterYearPeriodDefinition(Integer.parseInt(list[INDEX_COLUMN]), list[DISPLAY_NAME_COLUMN],
						Integer.parseInt(list[START_YEAR_COLUMN]), Integer.parseInt(list[END_YEAR_COLUMN])));
			}
			_waterYearPeriodDefinitions.sort(Comparator.comparing(WaterYearPeriodDefinition::getIndex));
		}
		catch(RuntimeException | CalLiteGUIException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to read water year index alias file: " + file, ex);
		}
	}

	public static final class WaterYearPeriodDefinition
	{
		private final int _index;
		private final String _name;
		private final int _startYear;
		private final int _endYear;

		private WaterYearPeriodDefinition(int index, String name, int startYear, int endYear)
		{
			_index = index;
			_name = name;
			_startYear = startYear;
			_endYear = endYear;
		}

		public String getName()
		{
			return _name;
		}

		private int getIndex()
		{
			return _index;
		}

		public int getStartYear()
		{
			return _startYear;
		}

		public int getEndYear()
		{
			return _endYear;
		}
	}
}

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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.WaterYearIndexModel;
import gov.ca.water.calgui.bo.WaterYearPeriod;
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
public class WaterYearIndexReader
{
	private static final Logger LOGGER = Logger.getLogger(WaterYearIndexReader.class.getName());
	private static final int INDEX_COL = 0;
	private static final int DISPLAY_NAME_COL = 1;

	private final List<WaterYearIndexDefinition> _waterYearIndexDefinitions = new ArrayList<>();
	private static WaterYearIndexReader instance;

	public static WaterYearIndexReader getInstance()
	{
		return instance;
	}

	public static void createInstance()
	{
		instance = new WaterYearIndexReader();
		instance.read();
	}

	public List<WaterYearIndexDefinition> getWaterYearIndexDefinitions()
	{
		return new ArrayList<>(_waterYearIndexDefinitions);
	}

	private void read()
	{
		_waterYearIndexDefinitions.clear();
		Path file = Paths.get(Constant.CONFIG_DIR).resolve(Constant.WATER_YEAR_INDEX_DEFINITIONS_FILE);
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		try
		{
			WaterYearNameLookup waterYearNameLookup = new WaterYearNameLookup();
			List<String> rows = fileSystemSvc.getFileData(file, true, FilePredicates.commentFilter());
			for(String row : rows)
			{
				String[] list = row.split(Constant.DELIMITER);
				String displayName = list[DISPLAY_NAME_COL];
				int waterYearIndexId = Integer.parseInt(list[INDEX_COL]);
				List<WaterYearPeriod> waterYearPeriods = waterYearNameLookup.getSortedWaterYearPeriods(waterYearIndexId);
				_waterYearIndexDefinitions.add(new WaterYearIndexDefinition(waterYearIndexId, displayName, waterYearPeriods));
			}
			_waterYearIndexDefinitions.sort(Comparator.comparing(WaterYearIndexDefinition::getWaterYearIndexId));
		}
		catch(RuntimeException | CalLiteGUIException | EpptInitializationException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to read water year index alias file: " + file, ex);
		}
	}

	public static final class WaterYearIndexDefinition
	{
		private final int _waterYearIndexId;
		private final String _displayName;
		private final List<WaterYearPeriod> _waterYearPeriods;

		private WaterYearIndexDefinition(int waterYearIndexId, String displayName, List<WaterYearPeriod> waterYearPeriods)
		{
			_waterYearIndexId = waterYearIndexId;
			_displayName = displayName;
			_waterYearPeriods = waterYearPeriods;
		}

		public String getDisplayName()
		{
			return _displayName;
		}

		public int getWaterYearIndexId()
		{
			return _waterYearIndexId;
		}

		@Override
		public String toString()
		{
			return _displayName;
		}

		public boolean matchesModel(WaterYearIndexModel index)
		{
			return index.getWaterYearIndexId() == getWaterYearIndexId();
		}

		public boolean matchesModel(int id)
		{
			return id == getWaterYearIndexId();
		}

		public List<WaterYearPeriod> getWaterYearPeriods()
		{
			return _waterYearPeriods;
		}
	}
}

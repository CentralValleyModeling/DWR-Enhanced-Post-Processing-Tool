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
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.WaterYearIndex;
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
public class WaterYearIndexAliasReader
{
	private static final Logger LOGGER = Logger.getLogger(WaterYearIndexAliasReader.class.getName());
	private static final String WYI_KEY = "WYI";
	private static final int KEY_COLUMN = 0;
	private static final int INDEX_COLUMN = 1;
	private static final int ALIAS_COLUMN = 2;
	private static final int DEFINITION_COLUMN = 3;

	private final List<WaterYearIndexAlias> _aliases = new ArrayList<>();
	private static WaterYearIndexAliasReader instance;

	public static WaterYearIndexAliasReader getInstance()
	{
		return instance;
	}

	public static void createInstance()
	{
		instance = new WaterYearIndexAliasReader();
		instance.read();
	}

	public List<WaterYearIndexAlias> getAliases()
	{
		return new ArrayList<>(_aliases);
	}

	private void read()
	{
		_aliases.clear();
		Path file = Paths.get(Constant.CONFIG_DIR).resolve("AnnualPeriodFilters.csv");
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		try
		{
			WaterYearNameLookup waterYearNameLookup = new WaterYearNameLookup();
			List<String> rows = fileSystemSvc.getFileData(file, true, FilePredicates.commentFilter());
			for(String row : rows)
			{
				String[] list = row.split(Constant.DELIMITER);
				if(!WYI_KEY.equals(list[KEY_COLUMN]))
				{
					continue;
				}
				Set<String> indexDefinitions = new HashSet<>();
				for(int i = DEFINITION_COLUMN; i < list.length; i++)
				{
					String index = list[i];
					if(!index.trim().isEmpty())
					{
						indexDefinitions.add(index);
					}
				}
				String alias = list[ALIAS_COLUMN];
				List<WaterYearPeriod> waterYearPeriods = waterYearNameLookup.getSortedWaterYearPeriods(alias);
				_aliases.add(new WaterYearIndexAlias(Integer.parseInt(list[INDEX_COLUMN]), alias, indexDefinitions, waterYearPeriods));
			}
			_aliases.sort(Comparator.comparing(WaterYearIndexAlias::getIndex));
		}
		catch(RuntimeException | CalLiteGUIException | EpptInitializationException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to read water year index alias file: " + file, ex);
		}
	}

	public static final class WaterYearIndexAlias
	{
		private final int _index;
		private final String _alias;
		private final Set<String> _waterYearIndexes;
		private final List<WaterYearPeriod> _waterYearPeriods;

		private WaterYearIndexAlias(int index, String alias, Set<String> waterYearIndexes, List<WaterYearPeriod> waterYearPeriods)
		{
			_index = index;
			_alias = alias;
			_waterYearIndexes = waterYearIndexes;
			_waterYearPeriods = waterYearPeriods.stream()
												.distinct()
												.collect(Collectors.toList());
		}

		public String getAlias()
		{
			return _alias;
		}

		private Set<String> getWaterYearIndexes()
		{
			return _waterYearIndexes;
		}

		private int getIndex()
		{
			return _index;
		}

		@Override
		public String toString()
		{
			return _alias;
		}

		public boolean isAliasFor(WaterYearIndex index)
		{
			return index.getName().equalsIgnoreCase(getAlias()) || getWaterYearIndexes().stream().map(String::trim).anyMatch(c -> c.equalsIgnoreCase(index.getName()));
		}

		public boolean isAliasFor(String index)
		{
			return index.equalsIgnoreCase(getAlias()) || getWaterYearIndexes().stream().map(String::trim).anyMatch(c -> c.equalsIgnoreCase(index));
		}

		public List<WaterYearPeriod> getWaterYearPeriods()
		{
			return _waterYearPeriods;
		}
	}
}

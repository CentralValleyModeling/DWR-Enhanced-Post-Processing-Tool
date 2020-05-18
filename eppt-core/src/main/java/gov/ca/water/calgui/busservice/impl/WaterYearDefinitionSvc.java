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

import java.nio.file.Paths;
import java.time.Month;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.time.temporal.ChronoUnit;
import java.time.temporal.IsoFields;
import java.time.temporal.TemporalField;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.ThresholdLinksBO;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IFileSystemSvc;
import gov.ca.water.calgui.techservice.impl.FilePredicates;
import gov.ca.water.calgui.techservice.impl.FileSystemSvcImpl;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;
import static gov.ca.water.calgui.constant.Constant.CSV_EXT;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-21-2019
 */
public class WaterYearDefinitionSvc
{
	private static final Logger LOGGER = Logger.getLogger(ThresholdLinksSeedDataSvc.class.getName());
	private static final String WATER_YEAR_DEFINITION_FILENAME = CONFIG_DIR + "/WaterYearDefinitions" + CSV_EXT;
	private static final int NAME = 0;
	private static final int START_MONTH = 1;
	private static final int END_MONTH = 2;
	private static WaterYearDefinitionSvc seedDataSvc;
	private final List<WaterYearDefinition> _definitions = new ArrayList<>();

	/**
	 * This will read the Threshold_Links.csv files and build the list and maps of {@link ThresholdLinksBO}
	 */
	private WaterYearDefinitionSvc() throws EpptInitializationException
	{
		LOGGER.fine("Building SeedDataSvcImpl Object.");
		initWaterYearDefinitionSvc();
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static WaterYearDefinitionSvc getWaterYearDefinitionSvc()
	{
		if(seedDataSvc == null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			return seedDataSvc;
		}
	}

	public List<WaterYearDefinition> getDefinitions()
	{
		return _definitions;
	}

	public static void createSeedDataSvcImplInstance() throws EpptInitializationException
	{
		if(seedDataSvc == null)
		{
			seedDataSvc = new WaterYearDefinitionSvc();
		}
	}

	private void initWaterYearDefinitionSvc() throws EpptInitializationException
	{
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		String errorStr = "";
		try
		{
			List<String> thresholdLinkStrings = fileSystemSvc.getFileData(Paths.get(WATER_YEAR_DEFINITION_FILENAME), true,
					FilePredicates.commentFilter());
			for(String thresholdLinkString : thresholdLinkStrings)
			{
				errorStr = thresholdLinkString;
				String[] list = thresholdLinkString.split(Constant.DELIMITER);
				if(list.length > END_MONTH && list[START_MONTH] != null && list[END_MONTH] != null)
				{
					String name = list[NAME];
					Month start = Month.of(DateTimeFormatter.ofPattern("MMMM").parse(list[START_MONTH]).get(ChronoField.MONTH_OF_YEAR));
					Month end = Month.of(DateTimeFormatter.ofPattern("MMMM").parse(list[END_MONTH]).get(ChronoField.MONTH_OF_YEAR));
					_definitions.add(new WaterYearDefinition(name, start, end));
				}
			}
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + WATER_YEAR_DEFINITION_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			LOGGER.log(Level.SEVERE, errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(NumberFormatException ex)
		{
			String errorMessage = "In file \"" + WATER_YEAR_DEFINITION_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The checkbox id is not an Integer " + ex.getMessage();
			LOGGER.log(Level.SEVERE, errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(IllegalArgumentException ex)
		{
			String errorMessage = "In file \"" + WATER_YEAR_DEFINITION_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The model could not be parsed " + ex.getMessage();
			LOGGER.log(Level.SEVERE, errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(CalLiteGUIException ex)
		{
			LOGGER.log(Level.SEVERE, ex.getMessage(), ex);
			throw new EpptInitializationException("Failed to get file data for file: " + WATER_YEAR_DEFINITION_FILENAME, ex);
		}
	}

}

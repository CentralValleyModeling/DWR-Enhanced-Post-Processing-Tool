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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.ThresholdLinksBO;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IFileSystemSvc;
import gov.ca.water.calgui.techservice.impl.FilePredicates;
import gov.ca.water.calgui.techservice.impl.FileSystemSvcImpl;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;
import static gov.ca.water.calgui.constant.Constant.CSV_EXT;

/**
 * This class holds key required data for the application from the
 * Threshold_Links*.csv files.
 *
 * @author Mohan
 */
public final class ThresholdLinksSeedDataSvc
{
	private static final Logger LOGGER = Logger.getLogger(ThresholdLinksSeedDataSvc.class.getName());
	private static final String THRESHOLD_LINKS_FILENAME = CONFIG_DIR + "/ThresholdLinks" + CSV_EXT;
	private static final int ID_INDEX = 0;
	private static final int MODEL_INDEX = 1;
	private static final int THRESHOLD_INDEX = 2;
	private static final int LABEL_INDEX = 3;
	private static ThresholdLinksSeedDataSvc seedDataSvc;
	private final Map<Integer, ThresholdLinksBO> _thresholdLinks = new HashMap<>();

	/**
	 * This will read the Threshold_Links.csv files and build the list and maps of {@link ThresholdLinksBO}
	 */
	private ThresholdLinksSeedDataSvc() throws EpptInitializationException
	{
		LOGGER.fine("Building SeedDataSvcImpl Object.");
		initThresholdLinksAllModels();
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static ThresholdLinksSeedDataSvc getSeedDataSvcImplInstance()
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

	public static void createSeedDataSvcImplInstance() throws EpptInitializationException
	{
		if(seedDataSvc == null)
		{
			seedDataSvc = new ThresholdLinksSeedDataSvc();
		}
	}

	private void initThresholdLinksAllModels() throws EpptInitializationException
	{
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		String errorStr = "";
		try
		{
			List<String> thresholdLinkStrings = fileSystemSvc.getFileData(Paths.get(THRESHOLD_LINKS_FILENAME), true,
					FilePredicates.commentFilter());
			//Header
			thresholdLinkStrings.remove(0);
			for(String thresholdLinkString : thresholdLinkStrings)
			{
				errorStr = thresholdLinkString;
				String[] list = thresholdLinkString.split(Constant.DELIMITER);
				if(list.length > THRESHOLD_INDEX && list[THRESHOLD_INDEX] != null)
				{
					int id = Integer.parseInt(list[ID_INDEX].trim());
					ThresholdLinksBO threadholdLinksBO = _thresholdLinks.computeIfAbsent(id,
							i -> createThresholdLinks(list, i));
					String model = list[MODEL_INDEX];
					String primary = list[THRESHOLD_INDEX].trim();
					threadholdLinksBO.addModelMapping(model, primary);
				}
			}
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + THRESHOLD_LINKS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			LOGGER.log(Level.SEVERE, errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(NumberFormatException ex)
		{
			String errorMessage = "In file \"" + THRESHOLD_LINKS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The checkbox id is not an Integer " + ex.getMessage();
			LOGGER.log(Level.SEVERE, errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(IllegalArgumentException ex)
		{
			String errorMessage = "In file \"" + THRESHOLD_LINKS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The model could not be parsed " + ex.getMessage();
			LOGGER.log(Level.SEVERE, errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(CalLiteGUIException ex)
		{
			LOGGER.log(Level.SEVERE, ex.getMessage(), ex);
			throw new EpptInitializationException("Failed to get file data for file: " + THRESHOLD_LINKS_FILENAME, ex);
		}
	}

	private ThresholdLinksBO createThresholdLinks(String[] list, int checkboxId)
	{
		String label = "";
		if(list.length > LABEL_INDEX)
		{
			label = list[LABEL_INDEX];
		}
		return new ThresholdLinksBO(checkboxId, label);
	}

	public ThresholdLinksBO getObjById(int id)
	{
		ThresholdLinksBO thresholdLinksBO = _thresholdLinks.get(id);
		if(thresholdLinksBO == null)
		{
			LOGGER.fine("There is no Threshold Link data for this id = " + id);
		}
		return thresholdLinksBO;
	}

}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bus_service.IGuiLinksSeedDataSvc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import gov.ca.water.calgui.tech_service.impl.FileSystemSvcImpl;
import org.apache.log4j.Logger;

import static gov.ca.water.calgui.constant.Constant.CONFIG_DIR;
import static gov.ca.water.calgui.constant.Constant.CSV_EXT;

/**
 * This class holds key required data for the application from the
 * GUI_Links*.csv files.
 * <p>
 * *
 *
 * @author Mohan
 */
public final class GuiLinksSeedDataSvcImpl implements IGuiLinksSeedDataSvc
{
	private static final Logger LOG = Logger.getLogger(GuiLinksSeedDataSvcImpl.class.getName());
	private static final String GUI_LINKS3_FILENAME = CONFIG_DIR + "/GUI_Links3" + CSV_EXT;
	private static final String GUI_LINKS_ALL_MODELS_FILENAME = CONFIG_DIR + "/GUI_Links_All_Models" + CSV_EXT;
	private static IGuiLinksSeedDataSvc seedDataSvc;
	private final Map<Integer, GUILinksAllModelsBO> _guiLinksAllModels = new HashMap<>();

	/**
	 * This will read the gui_links.csv files and build the list and maps of {@link GUILinksAllModelsBO}
	 */
	private GuiLinksSeedDataSvcImpl() throws EpptInitializationException
	{
		LOG.debug("Building SeedDataSvcImpl Object.");
		initGuiLinksAllModels();
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static IGuiLinksSeedDataSvc getSeedDataSvcImplInstance()
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
			seedDataSvc = new GuiLinksSeedDataSvcImpl();
		}
	}

	private void initGuiLinksAllModels() throws EpptInitializationException
	{
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		String errorStr = "";
		try
		{
			List<String> guiLinkStrings = fileSystemSvc.getFileData(GUI_LINKS_ALL_MODELS_FILENAME, true,
					GuiLinksSeedDataSvcImpl::isNotComments);
			for(String guiLinkString : guiLinkStrings)
			{
				errorStr = guiLinkString;
				String[] list = guiLinkString.split(Constant.DELIMITER);
				int checkboxId = Integer.parseInt(list[0].trim());
				GUILinksAllModelsBO guiLinksAllModelsBO = _guiLinksAllModels.computeIfAbsent(checkboxId,
						id -> createGuiLinksAllModels(list, id));
				String model = list[1];
				String primary = null;
				if(list.length > 2)
				{
					primary = list[2].trim();
				}
				String secondary = null;
				if(list.length > 3)
				{
					secondary = list[3].trim();
				}
				guiLinksAllModelsBO.addModelMapping(GUILinksAllModelsBO.Model.findModel(model), primary, secondary);
			}
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + GUI_LINKS_ALL_MODELS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			LOG.error(errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(NumberFormatException ex)
		{
			String errorMessage = "In file \"" + GUI_LINKS_ALL_MODELS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The checkbox id is not an Integer " + ex.getMessage();
			LOG.error(errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(IllegalArgumentException ex)
		{
			String errorMessage = "In file \"" + GUI_LINKS_ALL_MODELS_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The model could not be parsed " + ex.getMessage();
			LOG.error(errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex.getMessage(), ex);
			throw new EpptInitializationException("Failed to get file data for file: " + GUI_LINKS3_FILENAME, ex);
		}
	}

	private GUILinksAllModelsBO createGuiLinksAllModels(String[] list, int checkboxId)
	{
		String yTitle = "";
		if(list.length > 4)
		{
			yTitle = list[4];
		}
		String title = "";
		if(list.length > 5)
		{
			title = list[5];
		}
		String sLegend = "";
		if(list.length > 6)
		{
			sLegend = list[6];
		}
		String id = extractCheckboxId(checkboxId);
		return new GUILinksAllModelsBO(id, yTitle, title, sLegend);
	}

	private String extractCheckboxId(int checkboxId)
	{
		String id;
		try
		{
			id = String.format("ckbp%03d", checkboxId);
		}
		catch(NumberFormatException e)
		{
			id = Integer.toString(checkboxId);
		}
		return id;
	}

	private int extractCheckboxId(String checkboxId)
	{
		int id;
		try
		{
			id = Integer.parseInt(checkboxId.replace("ckbp", ""));
		}
		catch(NumberFormatException e)
		{
			id = Integer.parseInt(checkboxId);
		}
		return id;
	}

	/**
	 * This will tell whether the line is comment or not.
	 *
	 * @param line The line to be checked.
	 * @return Will return true if the line id not comment.
	 */
	private static boolean isNotComments(String line)
	{
		return !line.startsWith(Constant.EXCLAMATION);
	}

	public GUILinksAllModelsBO getObjById(String id)
	{
		GUILinksAllModelsBO guiLinks3BO = _guiLinksAllModels.get(extractCheckboxId(id));
		if(guiLinks3BO == null)
		{
			LOG.info("There is no GUI Links data for this id = " + id);
		}
		return guiLinks3BO;
	}

}

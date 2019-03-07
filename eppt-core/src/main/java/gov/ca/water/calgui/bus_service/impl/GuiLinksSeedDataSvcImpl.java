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
import gov.ca.water.calgui.bo.GUILinks2BO;
import gov.ca.water.calgui.bo.GUILinks3BO;
import gov.ca.water.calgui.bo.GUILinks4BO;
import gov.ca.water.calgui.bus_service.IGuiLinksSeedDataSvc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import gov.ca.water.calgui.tech_service.impl.FileSystemSvcImpl;
import org.apache.log4j.Logger;

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
	private static final String GUI_LINKS3_FILENAME = "eppt/modules/Config/GUI_Links3" + CSV_EXT;
	private static IGuiLinksSeedDataSvc _seedDataSvc;

	private final Map<String, GUILinks3BO> _guiLinks3Map = new HashMap<>();

	/**
	 * This will read the gui_links.csv files and build the list and maps of
	 * {@link GUILinks2BO} objects and the maps of {@link GUILinks4BO} and
	 * {@link GUILinks4BO} objects.
	 */
	private GuiLinksSeedDataSvcImpl() throws EpptInitializationException
	{
		LOG.debug("Building SeedDataSvcImpl Object.");
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		initGuiLinks3(fileSystemSvc);
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
		if(_seedDataSvc == null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			return _seedDataSvc;
		}
	}

	public static IGuiLinksSeedDataSvc createSeedDataSvcImplInstance() throws EpptInitializationException
	{
		if(_seedDataSvc == null)
		{
			_seedDataSvc = new GuiLinksSeedDataSvcImpl();
		}
		return _seedDataSvc;
	}

	private void initGuiLinks3(IFileSystemSvc fileSystemSvc) throws EpptInitializationException
	{
		String errorStr = "";
		try
		{
			List<String> guiLinks3StrList = fileSystemSvc.getFileData(GUI_LINKS3_FILENAME, true,
					GuiLinksSeedDataSvcImpl::isNotComments);
			for(String guiLink3Str : guiLinks3StrList)
			{
				errorStr = guiLink3Str;
				String[] list = guiLink3Str.split(Constant.DELIMITER);
				GUILinks3BO guiLinks3BO = new GUILinks3BO(list[0], list[1], list[2], list[3], list[4], list[5]);
				String id = extractCheckboxId(list);
				_guiLinks3Map.put(id, guiLinks3BO);
			}

		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + GUI_LINKS3_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			LOG.error(errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex.getMessage(), ex);
			throw new EpptInitializationException("Failed to get file data for file: " + GUI_LINKS3_FILENAME, ex);
		}
	}

	private String extractCheckboxId(String[] list)
	{
		String id;
		try
		{
			id = String.format("ckbp%03d", Integer.parseInt(list[0]));
		}
		catch(NumberFormatException e)
		{
			id = list[0];
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

	public GUILinks3BO getObjById(String id)
	{
		GUILinks3BO guiLinks3BO = _guiLinks3Map.get(id);
		if(guiLinks3BO == null)
		{
			LOG.info("There is no GUI_Links3 data for this id = " + id);
		}
		return guiLinks3BO;
	}

}

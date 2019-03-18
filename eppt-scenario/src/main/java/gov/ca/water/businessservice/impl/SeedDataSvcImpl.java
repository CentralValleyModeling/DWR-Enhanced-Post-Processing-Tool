/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import gov.ca.water.bo.GUILinks2BO;
import gov.ca.water.bo.GUILinks4BO;
import gov.ca.water.businessservice.ISeedDataSvc;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import gov.ca.water.calgui.tech_service.impl.FileSystemSvcImpl;
import org.apache.log4j.Logger;

import static gov.ca.water.calgui.constant.Constant.CSV_EXT;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-06-2019
 */
public class SeedDataSvcImpl implements ISeedDataSvc
{
	private static final Logger LOG = Logger.getLogger(SeedDataSvcImpl.class.getName());
	private static final String GUI_LINKS2_FILENAME = System.getProperty("user.dir") + "//Config//GUI_Links2" + CSV_EXT;
	private static final String GUI_LINKS4_FILENAME = System.getProperty("user.dir") + "//Config//GUI_Links4" + CSV_EXT;
	private static ISeedDataSvc _seedDataSvc;
	private final List<GUILinks2BO> _guiLinks2BOList = new ArrayList<>();
	private final Map<String, GUILinks2BO> _guiIdMap = new HashMap<>();
	private final Map<String, GUILinks2BO> _tableIdMap = new HashMap<>();
	private final Map<String, GUILinks2BO> _regIdMap = new HashMap<>();
	private final Map<String, GUILinks4BO> _guiLinks4Map = new HashMap<>();

	/**
	 * This will read the gui_links.csv files and build the list and maps of
	 * {@link GUILinks2BO} objects and the maps of {@link GUILinks4BO} and
	 * {@link GUILinks4BO} objects.
	 */
	private SeedDataSvcImpl() throws EpptInitializationException
	{
		LOG.debug("Building SeedDataSvcImpl Object.");
		IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
		initGuiLinks2(fileSystemSvc);
		initGuiLinks4(fileSystemSvc);
	}

	public static ISeedDataSvc createSeedDataSvcImplInstance() throws EpptInitializationException
	{
		if(_seedDataSvc == null)
		{
			_seedDataSvc = new SeedDataSvcImpl();
		}
		return _seedDataSvc;
	}


	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static ISeedDataSvc getSeedDataSvcImplInstance()
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

	@Override
	public Map<String, GUILinks2BO> getTableIdMap()
	{
		return _tableIdMap;
	}

	@Override
	public boolean hasSeedDataObject(String guiId)
	{
		return this.getObjByGuiId(guiId) != null;
	}

	@Override
	public List<GUILinks2BO> getRegulationsTabData()
	{
		return this._regIdMap.values().stream()
							 .filter(seedData -> seedData.getDashboard().equalsIgnoreCase(Constant.REGULATIONS_TABNAME))
							 .collect(Collectors.toList());
	}

	@Override
	public GUILinks2BO getObjByGuiId(String guiId)
	{
		GUILinks2BO gUILinks2BO = _guiIdMap.get(guiId);
		if(gUILinks2BO == null)
		{
			LOG.info("There is no GUI_Links2 data for this guiId = " + guiId);
		}
		return gUILinks2BO;
	}

	@Override
	public GUILinks4BO getObjByRunBasisLodCcprojCcmodelIds(String id)
	{
		GUILinks4BO gUILinks4BO = this._guiLinks4Map.get(id);
		if(gUILinks4BO == null)
		{
			LOG.info("There is no GuiLinks4BO Object for this value = " + id);
		}
		return gUILinks4BO;
	}

	@Override
	public List<GUILinks2BO> getUserTables()
	{
		return new ArrayList<>(this._tableIdMap.values());
	}

	@Override
	public List<GUILinks2BO> getGUILinks2BOList()
	{
		return _guiLinks2BOList;
	}

	private String initGuiLinks4(IFileSystemSvc fileSystemSvc) throws EpptInitializationException
	{
		String errorStr = "";
		try
		{
			List<String> guiLinks4StrList = fileSystemSvc.getFileData(GUI_LINKS4_FILENAME, true,
					SeedDataSvcImpl::isNotComments);
			for(String guiLink4Str : guiLinks4StrList)
			{
				errorStr = guiLink4Str;
				String[] list = guiLink4Str.split(Constant.DELIMITER);
				GUILinks4BO gUILinks4BO = new GUILinks4BO(list[0], list[1], list[2], list[3], list[4], list[5], list[6],
						list[7], list[8], list[9], list[10]);
				String id = gUILinks4BO.getRunBasisID() + gUILinks4BO.getLodId() + gUILinks4BO.getCcprojectId()
						+ gUILinks4BO.getCcmodelId();
				_guiLinks4Map.put(id, gUILinks4BO);
			}

		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + GUI_LINKS4_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			LOG.error(errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex.getMessage(), ex);
			throw new EpptInitializationException("Failed to get file data for file: " + GUI_LINKS4_FILENAME, ex);
		}
		return errorStr;
	}

	private String initGuiLinks2(IFileSystemSvc fileSystemSvc) throws EpptInitializationException
	{
		String errorStr = "";
		try
		{
			List<String> guiLinks2StrList = fileSystemSvc.getFileData(GUI_LINKS2_FILENAME, true,
					SeedDataSvcImpl::isNotComments);
			for(String guiLinks2Str : guiLinks2StrList)
			{
				errorStr = guiLinks2Str;
				String[] list = guiLinks2Str.split(Constant.DELIMITER);
				GUILinks2BO gUILinks2BO = new GUILinks2BO(list[0], list[1], list[2], list[3], list[4], list[5], list[6],
						list[7], list[8], list[9], list[10], list[11], list[12], list[13], list[14]);
				_guiLinks2BOList.add(gUILinks2BO);
				_guiIdMap.put(gUILinks2BO.getGuiId(), gUILinks2BO);
				if(!gUILinks2BO.getTableID().equals(Constant.N_A))
				{
					if(_tableIdMap.get(gUILinks2BO.getTableID()) == null)
					{
						_tableIdMap.put(gUILinks2BO.getTableID(), gUILinks2BO);
					}
					else
					{
						String errorMsg = "The Table Id is same for these two controls - " + _tableIdMap.get(
								gUILinks2BO.getTableID()).getGuiId() + " , " + gUILinks2BO.getGuiId();
						throw new EpptInitializationException(errorMsg);
					}
				}
				if(!gUILinks2BO.getRegID().equals(Constant.N_A))
				{
					if(_regIdMap.get(gUILinks2BO.getRegID()) == null)
					{
						_regIdMap.put(gUILinks2BO.getRegID(), gUILinks2BO);
					}
					else
					{
						throw new EpptInitializationException("The RegId is same for these two controls - "
								+ _regIdMap.get(gUILinks2BO.getRegID()).getGuiId() + " , "
								+ gUILinks2BO.getGuiId());
					}
				}
			}
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + GUI_LINKS2_FILENAME + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			LOG.error(errorMessage, ex);
			throw new EpptInitializationException(errorMessage);
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex.getMessage(), ex);
			throw new EpptInitializationException("Failed to get file data for file: " + GUI_LINKS2_FILENAME, ex);
		}
		return errorStr;
	}
}

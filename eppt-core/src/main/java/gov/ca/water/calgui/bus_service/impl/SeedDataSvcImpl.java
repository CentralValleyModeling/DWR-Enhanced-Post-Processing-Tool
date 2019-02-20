/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.GUILinks2BO;
import gov.ca.water.calgui.bo.GUILinks3BO;
import gov.ca.water.calgui.bo.GUILinks4BO;
import gov.ca.water.calgui.bus_service.ISeedDataSvc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import gov.ca.water.calgui.tech_service.impl.FileSystemSvcImpl;
import org.apache.log4j.Logger;

/**
 * This class holds key required data for the application from the
 * GUI_Links*.csv files.
 * <p>
 * *
 *
 * @author Mohan
 */
public final class SeedDataSvcImpl implements ISeedDataSvc
{
	private static final Logger LOG = Logger.getLogger(SeedDataSvcImpl.class.getName());
	private static ISeedDataSvc seedDataSvc;
	private IErrorHandlingSvc errorHandlingSvc;
	private IFileSystemSvc fileSystemSvc;
	private List<GUILinks2BO> guiLinks2BOList;
	private List<GUILinks3BO> guiLinks3BOList;
	private List<GUILinks4BO> guiLinks4BOList;

	private String[][] gl3_lookups;

	private Map<String, GUILinks2BO> guiIdMap;
	private Map<String, GUILinks2BO> tableIdMap;
	private Map<String, GUILinks2BO> regIdMap;
	private Map<String, GUILinks3BO> guiLinks3Map;
	private Map<String, GUILinks4BO> guiLinks4Map;

	/**
	 * This will read the gui_links.csv files and build the list and maps of
	 * {@link GUILinks2BO} objects and the maps of {@link GUILinks4BO} and
	 * {@link GUILinks4BO} objects.
	 */
	private SeedDataSvcImpl()
	{
		LOG.debug("Building SeedDataSvcImpl Object.");
		this.errorHandlingSvc = new ErrorHandlingSvcImpl();
		this.fileSystemSvc = new FileSystemSvcImpl();
		this.guiLinks2BOList = new ArrayList<GUILinks2BO>();
		this.guiLinks4BOList = new ArrayList<GUILinks4BO>();
		this.guiIdMap = new HashMap<String, GUILinks2BO>();
		this.tableIdMap = new HashMap<String, GUILinks2BO>();
		this.regIdMap = new HashMap<String, GUILinks2BO>();
		this.guiLinks3Map = new HashMap<String, GUILinks3BO>();
		this.guiLinks4Map = new HashMap<String, GUILinks4BO>();

		List<String> guiLinks2StrList;
		List<String> guiLinks3StrList;
		List<String> guiLinks4StrList;
		String errorStr = "";
		String fileName = "";
		try
		{
			fileName = Constant.GUI_LINKS2_FILENAME;
			guiLinks2StrList = fileSystemSvc.getFileData(fileName, true, SeedDataSvcImpl::isNotComments);
			for(String guiLinks2Str : guiLinks2StrList)
			{
				errorStr = guiLinks2Str;
				String[] list = guiLinks2Str.split(Constant.DELIMITER);
				GUILinks2BO gUILinks2BO = new GUILinks2BO(list[0], list[1], list[2], list[3], list[4], list[5], list[6],
						list[7], list[8], list[9], list[10], list[11], list[12], list[13], list[14]);
				guiLinks2BOList.add(gUILinks2BO);
				guiIdMap.put(gUILinks2BO.getGuiId(), gUILinks2BO);
				if(!gUILinks2BO.getTableID().equals(Constant.N_A))
				{
					if(tableIdMap.get(gUILinks2BO.getTableID()) == null)
					{
						tableIdMap.put(gUILinks2BO.getTableID(), gUILinks2BO);
					}
					else
					{
						errorHandlingSvc.displayErrorMessageBeforeTheUI(
								new CalLiteGUIException("The Table Id is same for these two controls - "
										+ tableIdMap.get(gUILinks2BO.getTableID()).getGuiId() + " , "
										+ gUILinks2BO.getGuiId(), true));
					}
				}
				if(!gUILinks2BO.getRegID().equals(Constant.N_A))
				{
					if(regIdMap.get(gUILinks2BO.getRegID()) == null)
					{
						regIdMap.put(gUILinks2BO.getRegID(), gUILinks2BO);
					}
					else
					{
						errorHandlingSvc.displayErrorMessageBeforeTheUI(
								new CalLiteGUIException("The RegId is same for these two controls - "
										+ regIdMap.get(gUILinks2BO.getRegID()).getGuiId() + " , "
										+ gUILinks2BO.getGuiId(), true));
					}
				}
			}
			fileName = Constant.GUI_LINKS4_FILENAME;
			guiLinks4StrList = fileSystemSvc.getFileData(fileName, true, SeedDataSvcImpl::isNotComments);
			for(String guiLink4Str : guiLinks4StrList)
			{
				errorStr = guiLink4Str;
				String[] list = guiLink4Str.split(Constant.DELIMITER);
				GUILinks4BO gUILinks4BO = new GUILinks4BO(list[0], list[1], list[2], list[3], list[4], list[5], list[6],
						list[7], list[8], list[9], list[10]);
				guiLinks4BOList.add(gUILinks4BO);
				String id = gUILinks4BO.getRunBasisID() + gUILinks4BO.getLodId() + gUILinks4BO.getCcprojectId()
						+ gUILinks4BO.getCcmodelId();
				guiLinks4Map.put(id, gUILinks4BO);
			}

			fileName = Constant.GUI_LINKS3_FILENAME;
			guiLinks3StrList = fileSystemSvc.getFileData(fileName, true, SeedDataSvcImpl::isNotComments);
			gl3_lookups = new String[guiLinks3StrList.size()][6];
			for(String guiLink3Str : guiLinks3StrList)
			{
				errorStr = guiLink3Str;
				String[] list = guiLink3Str.split(Constant.DELIMITER);
				GUILinks3BO guiLinks3BO = new GUILinks3BO(list[0], list[1], list[2], list[3], list[4], list[5]);
				String id;
				try
				{
					id = String.format("ckbp%03d", Integer.parseInt(list[0]));
				}
				catch(NumberFormatException e)
				{
					id = list[0];
				}
				guiLinks3Map.put(id, guiLinks3BO);
			}

		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			String errorMessage = "In file \"" + fileName + "\" has corrupted data at line \"" + errorStr + "\""
					+ Constant.NEW_LINE + "The column number which the data is corrupted is " + ex.getMessage();
			LOG.error(errorMessage, ex);
			errorHandlingSvc.displayErrorMessageBeforeTheUI(new CalLiteGUIException(errorMessage, ex, true));
		}
		catch(CalLiteGUIException ex)
		{
			LOG.error(ex.getMessage(), ex);
			errorHandlingSvc.displayErrorMessageBeforeTheUI(ex);
		}
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
		if(seedDataSvc == null)
		{
			seedDataSvc = new SeedDataSvcImpl();
		}
		return seedDataSvc;
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
	public GUILinks2BO getObjByGuiId(String guiId)
	{
		GUILinks2BO gUILinks2BO = guiIdMap.get(guiId);
		if(gUILinks2BO == null)
		{
			LOG.info("There is no GUI_Links2 data for this guiId = " + guiId);
		}
		return gUILinks2BO;
	}

	public GUILinks3BO getObjById(String id)
	{
		GUILinks3BO guiLinks3BO = guiLinks3Map.get(id);
		if(guiLinks3BO == null)
		{
			LOG.info("There is no GUI_Links3 data for this id = " + id);
		}
		return guiLinks3BO;
	}

	@Override
	public GUILinks4BO getObjByRunBasisLodCcprojCcmodelIds(String id)
	{
		GUILinks4BO gUILinks4BO = this.guiLinks4Map.get(id);
		if(gUILinks4BO == null)
		{
			LOG.info("There is no GuiLinks4BO Object for this value = " + id);
		}
		return gUILinks4BO;
	}

	@Override
	public List<GUILinks2BO> getUserTables()
	{
		return this.tableIdMap.values().stream().collect(Collectors.toList());
	}

	@Override
	public List<GUILinks2BO> getGUILinks2BOList()
	{
		return guiLinks2BOList;
	}

	@Override
	public Map<String, GUILinks2BO> getTableIdMap()
	{
		return tableIdMap;
	}

	@Override
	public boolean hasSeedDataObject(String guiId)
	{
		return this.getObjByGuiId(guiId) != null;
	}

	@Override
	public List<GUILinks2BO> getRegulationsTabData()
	{
		return this.regIdMap.values().stream()
							.filter(seedData -> seedData.getDashboard().equalsIgnoreCase(Constant.REGULATIONS_TABNAME))
							.collect(Collectors.toList());
	}
}

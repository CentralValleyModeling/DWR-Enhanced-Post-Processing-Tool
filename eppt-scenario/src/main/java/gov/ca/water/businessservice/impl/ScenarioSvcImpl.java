/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.businessservice.impl;

import java.awt.Component;
import java.awt.Container;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.Set;
import javax.swing.*;
import javax.swing.text.JTextComponent;

import gov.ca.water.bo.GUILinks2BO;
import gov.ca.water.bo.NumericTextField;
import gov.ca.water.businessservice.IScenarioSvc;
import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.CalLiteGUIException;
import gov.ca.water.calgui.bo.DataTableModel;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.IFileSystemSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import gov.ca.water.calgui.tech_service.impl.FileSystemSvcImpl;
import org.apache.commons.io.FileDeleteStrategy;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.log4j.Logger;
import org.swixml.SwingEngine;
import org.swixml.XScrollPane;
import wrimsv2.evaluator.TimeOperation;

/**
 * This is the class for handling the cls file and saving the data.
 *
 * @author Mohan
 */
public final class ScenarioSvcImpl implements IScenarioSvc
{
	private static final Logger LOG = Logger.getLogger(ScenarioSvcImpl.class.getName());
	private static IScenarioSvc scenarioSvc;
	private IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
	private IFileSystemSvc fileSystemSvc = new FileSystemSvcImpl();
	private int[] regulationoptions = new int[100];
	private Map<String, DataTableModel> userDefinedTableMap = new HashMap<>();
	private boolean isCLSFlag = true;

	private ScenarioSvcImpl()
	{

	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static IScenarioSvc getScenarioSvcImplInstance()
	{
		if(scenarioSvc == null)
		{
			scenarioSvc = new ScenarioSvcImpl();
		}
		return scenarioSvc;
	}

	/**
	 * Convert the month name to the int value of it.
	 *
	 * @param month The month name which you want to convert.
	 * @return The int value of the month.
	 */
	public static int monthToInt(String month)
	{
		HashMap<String, Integer> monthMap = new HashMap<String, Integer>();
		monthMap.put("jan", 1);
		monthMap.put("feb", 2);
		monthMap.put("mar", 3);
		monthMap.put("apr", 4);
		monthMap.put("may", 5);
		monthMap.put("jun", 6);
		monthMap.put("jul", 7);
		monthMap.put("aug", 8);
		monthMap.put("sep", 9);
		monthMap.put("oct", 10);
		monthMap.put("nov", 11);
		monthMap.put("dec", 12);
		month = month.toLowerCase();
		Integer monthCode = null;
		monthCode = monthMap.get(month);
		return monthCode == null ? -1 : monthCode.intValue();
	}

	/**
	 * This will tell whether the line is comment or not.
	 *
	 * @param line the line to be checked.
	 * @return Will tell whether the line is comment or not.
	 */
	private static boolean isComment(String line)
	{
		return line.startsWith(Constant.EXCLAMATION);
	}

	@Override
	public void getCLSData(String fileName, List<String> controlStrList, List<String> dataTableModelStrList,
						   List<String> regulationoptionsStr, List<String> wsidiStatusStr)
			throws EpptInitializationException
	{
		List<String> data;
		boolean isDataTableModel = false;
		boolean isRegulationoptions = false;
		boolean isWSIDIStatus = false;
		try
		{
			data = fileSystemSvc.getFileData(fileName, true);
		}
		catch(CalLiteGUIException ex)
		{
			throw new EpptInitializationException("Error trying to get CLS data from file: " + fileName, ex);
		}
		for(String stringInClsFile : data)
		{
			switch(stringInClsFile)
			{
				case "DATATABLEMODELS":
					isDataTableModel = true;
					continue;
				case "END DATATABLEMODELS":
					isDataTableModel = false;
					continue;
				case "REGULATIONOPTIONS":
					isRegulationoptions = true;
					continue;
				case "END REGULATIONOPTIONS":
					isRegulationoptions = false;
					continue;
				case "WSIDILABEL":
					isWSIDIStatus = true;
					continue;
				case "END WSIDILABEL":
					isWSIDIStatus = false;
					continue;
			}

			if(isDataTableModel)
			{
				dataTableModelStrList.add(stringInClsFile);
			}
			else if(isRegulationoptions)
			{
				regulationoptionsStr.add(stringInClsFile);
			}
			else if(isWSIDIStatus)
			{
				wsidiStatusStr.add(stringInClsFile);
			}
			else
			{
				controlStrList.add(stringInClsFile);
			}
		}
	}

	@Override
	public void applyClsFile(String fileName, SwingEngine swingEngine, Map<String, GUILinks2BO> tableMap)
			throws EpptInitializationException
	{
		this.isCLSFlag = true;
		List<String> controlStrList = new ArrayList<>();
		List<String> dataTableModelStrList = new ArrayList<>();
		List<String> regulationoptionsStr = new ArrayList<>();
		List<String> wsidiStatusStr = new ArrayList<>();
		// Read in the cls file data.
		scenarioSvc.getCLSData(fileName, controlStrList, dataTableModelStrList, regulationoptionsStr, wsidiStatusStr);
		if(!regulationoptionsStr.isEmpty())
		{
			List<String> regData = Arrays.asList(regulationoptionsStr.get(0).split(Constant.PIPELINE_DELIMITER));
			for(int i = 0; i < regData.size(); i++)
			{
				this.regulationoptions[i] = Integer.parseInt(regData.get(i));
			}
		}
		if(!dataTableModelStrList.isEmpty())
		{
			populateClsTableMap(dataTableModelStrList, tableMap);
		}

		applyControls(controlStrList, swingEngine);
		if(!wsidiStatusStr.isEmpty())
		{
			JLabel l = (JLabel) swingEngine.find("op_WSIDI_Status");
			l.setText(wsidiStatusStr.get(0));
		}
		this.isCLSFlag = false;
	}

	@Override
	public boolean save(String fileName, SwingEngine swingEngine, List<GUILinks2BO> guiLinks2BOList)
	{
		try
		{
			saveToCLSFile(Constant.SCENARIOS_DIR + fileName + Constant.CLS_EXT, swingEngine, guiLinks2BOList);
			saveFiles(fileName, swingEngine, guiLinks2BOList);
			return true;
		}
		catch(CalLiteGUIException ex)
		{
			errorHandlingSvc.businessErrorHandler(new CalLiteGUIException("We are unable to save the file.", ex));
			return false;
		}
	}

	@Override
	public boolean hasUserDefinedTable(String tableName)
	{
		return this.userDefinedTableMap.get(tableName) != null;
	}

	@Override
	public void removeUserDefinedTable(String tableName)
	{
		if(this.hasUserDefinedTable(tableName))
		{
			this.userDefinedTableMap.remove(tableName);
		}
	}

	@Override
	public DataTableModel getUserDefinedTable(String tableName)
	{
		return this.userDefinedTableMap.get(tableName);
	}

	@Override
	public void addUserDefinedTable(String tableName, DataTableModel dataTableModel)
	{
		this.userDefinedTableMap.put(tableName, dataTableModel);
	}

	@Override
	public int[] getRegulationoptions()
	{
		return regulationoptions;
	}

	@Override
	public boolean isCLSFileLoading()
	{
		return this.isCLSFlag;
	}

	/**
	 * This method is used to convert the table data in the cls file to the user
	 * defined table map.
	 *
	 * @param dataTableModelStrList This data table strings from the cls file.
	 * @param tableMap              The map with key as the table id and value as table object.
	 */
	private void populateClsTableMap(List<String> dataTableModelStrList, Map<String, GUILinks2BO> tableMap)
			throws EpptInitializationException
	{
		String tableName;
		for(String dataTableModelStr : dataTableModelStrList)
		{
			try
			{
				String[] strArr = dataTableModelStr.split(Constant.PIPELINE_DELIMITER);
				String tableId = strArr[0];
				if(!(tableId.equals("9") || tableId.equals("10") || tableId.equals("5")))
				{
					if(tableMap.get(strArr[0]) == null)
					{
						tableName = strArr[0];
					}
					else
					{
						tableName = tableMap.get(tableId).getDataTables();
					}
					String[] columnNames = getColumnNamesFromTableId(tableName);
					userDefinedTableMap.put(tableName,
							new DataTableModel(tableName, columnNames, getTableDataFromCLSFile(strArr[1]), true));
				}
				else if(tableId.equals("5"))
				{
					tableName = tableMap.get(tableId).getDataTables();
					String[] tableNames = tableName.split(Constant.PIPELINE_DELIMITER);
					String[] columnNames1;
					String[] columnNames2;
					if(tableNames[0].equals("gui_x2active"))
					{
						columnNames1 = getColumnNamesFromTableId(tableNames[0]);
						columnNames2 = getColumnNamesFromTableId(tableNames[1]);
					}
					else
					{
						columnNames1 = getColumnNamesFromTableId(tableNames[1]);
						columnNames2 = getColumnNamesFromTableId(tableNames[0]);
					}
					String[] newColumnNames = {columnNames1[0], columnNames1[1], columnNames2[1], columnNames2[2],
							columnNames2[3], columnNames2[4], columnNames2[5]};
					userDefinedTableMap.put(tableName,
							new DataTableModel(tableName, newColumnNames, getTableDataFromCLSFile(strArr[1]), true));
				}
				else
				{
					tableName = tableMap.get(strArr[0]).getDataTables();
					String[] columnNames = new String[2];
					columnNames[0] = "wsi";
					columnNames[1] = "di";
					DataTableModel dtm = new DataTableModel(tableName, columnNames, getTableDataFromCLSFile(strArr[1]),
							true);
					// We are setting the table name to user defined because it
					// is coming from the cls file.
					dtm.setTableName(Constant.USER_DEFINED);
					userDefinedTableMap.put(tableName, dtm);
				}
			}
			catch(CalLiteGUIException ex)
			{
				throw new EpptInitializationException("Error attempting to get column name from table id.", ex);
			}
		}
	}

	/**
	 * This will convert the table string from the cls file into the array.
	 *
	 * @param data Table data string.
	 * @return Will return the array of table data.
	 */
	private Object[][] getTableDataFromCLSFile(String data)
	{
		String[] tableDataArr = data.split(Constant.SEMICOLON);
		int noOfRows = tableDataArr.length;
		int noOfCol = tableDataArr[0].split(Constant.DELIMITER).length;
		Object[][] tableData = new Object[noOfRows][noOfCol];
		for(int i = 0; i < tableDataArr.length; i++)
		{
			String[] colData = tableDataArr[i].split(Constant.DELIMITER);
			for(int j = 0; j < colData.length; j++)
			{
				tableData[i][j] = colData[j];
			}
		}
		return tableData;
	}

	/**
	 * This method is used to get the column name for the table from the table
	 * name.
	 *
	 * @param tableName Just the table name as gui_link2.table
	 * @return Will return the table names.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private String[] getColumnNamesFromTableId(String tableName) throws CalLiteGUIException
	{
		List<String> tableStrList = fileSystemSvc
				.getFileDataForTables(Constant.MODEL_W2_WRESL_LOOKUP_DIR + tableName + Constant.TABLE_EXT);
		Optional<String> header = Optional.empty();
		try
		{

			header = tableStrList.stream().filter(obj -> obj.contains(Constant.HEADERS)).findFirst();
		}
		catch(NoSuchElementException ex)
		{
			throw new CalLiteGUIException(
					"The table name is " + tableName + Constant.NEW_LINE
							+ "The Header is missing or not been formatted correctly in the table." + Constant.NEW_LINE,
					ex);
		}
		String[] headers = new String[0];
		if(header.isPresent())
		{
			String[] da = header.get().split(Constant.OLD_DELIMITER);

			headers = new String[da.length - 1];
			for(int i = 0; i < headers.length; i++)
			{
				headers[i] = da[i + 1];
			}
		}
		return headers;
	}

	/**
	 * This will take the control strings from the cls file and apply it to the
	 * current ui.
	 *
	 * @param controlStrList Control strings from the cls file
	 * @param swingEngine    The object of the GUI.
	 */
	private void applyControls(List<String> controlStrList, SwingEngine swingEngine)
	{
		for(String controlStr : controlStrList)
		{
			String[] comArr = controlStr.split(Constant.PIPELINE_DELIMITER);
			if(comArr.length < 1)
			{
				LOG.info("the line in the cls file is in wrong formate. the line is \"" + controlStr + "\"");
				continue;
			}
			String compName = comArr[0];
			String value = comArr[1];
			JComponent component = (JComponent) swingEngine.find(compName);

			if(component == null)
			{
				LOG.error("Not found: " + compName);
			}
			else
			{
				if(component instanceof JCheckBox || component instanceof JRadioButton)
				{
					((AbstractButton) component).setSelected(Boolean.parseBoolean(value));
				}
				else if(component instanceof JSpinner)
				{
					JSpinner spn = (JSpinner) component;
					if(value.matches("((-|\\+)?[0-9])+"))
					{
						int val1 = Integer.parseInt(value);
						spn.setValue(val1);
					}
					else
					{
						spn.setValue(value);
					}
				}
				else
				{
					if(component != null)
					{
						((JTextComponent) component).setText(value.replace("~~", "\n"));
					}
				}
			}

		}
	}

	/**
	 * This will delete all the Files and Directory.
	 *
	 * @param dirAbsPath The directory path.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private void deleteDirAndFiles(File dirAbsPath) throws CalLiteGUIException
	{
		if(dirAbsPath.listFiles() != null)
		{
			String path = "";
			for(File file : dirAbsPath.listFiles())
			{
				try
				{
					path = dirAbsPath.getAbsolutePath();
					FileDeleteStrategy.FORCE.delete(file);
				}
				catch(IOException ex)
				{
					throw new CalLiteGUIException(
							"We had a problem when deleting the directory. The directory is " + path, ex);
				}
			}
		}
	}

	/**
	 * This will save the current ui state to the tables and scenario directory.
	 *
	 * @param fileName        Just the file name with out the path and extension.
	 * @param swingEngine     The object of the GUI.
	 * @param guiLinks2BOList The data list from gui_link2.table.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private void saveFiles(String fileName, SwingEngine swingEngine, List<GUILinks2BO> guiLinks2BOList)
			throws CalLiteGUIException
	{
		String runDirAbsPath = Paths.get(Constant.RUN_DETAILS_DIR + fileName + Constant.RUN_DIR).toString();
		String generatedDirAbsPath = Paths.get(Constant.RUN_DETAILS_DIR + fileName + Constant.GENERATED_DIR).toString();
		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT,
				"Creating the directory and copying the files.");

		if(!Files.isExecutable(Paths.get(Constant.RUN_DETAILS_DIR + fileName)))
		{
			createDirAndCopyFiles(Constant.MODEL_W2_WRESL_DIR, runDirAbsPath);
		}
		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT, "Deleting the previous data.");
		File generatedDir = new File(generatedDirAbsPath);
		// deleting all directory and files under Generate directory.
		deleteDirAndFiles(generatedDir);
		// create DSS, Lookup, and external folders
		generatedDir = new File(generatedDirAbsPath, "DSS");
		generatedDir.mkdirs();
		generatedDir = new File(generatedDirAbsPath, "Lookup");
		generatedDir.mkdirs();
		generatedDir = new File(generatedDirAbsPath, "External");
		generatedDir.mkdirs();

		File runDir = new File(runDirAbsPath + "//DSS");
		deleteDirAndFiles(runDir);

		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT, "Copying DSS Files.");
		// Copy DSS files to "Generated" folder
		copyDSSFileToScenarioDirectory(generatedDirAbsPath, ((JTextField) swingEngine.find("hyd_DSS_SV")).getText());
		copyDSSFileToScenarioDirectory(generatedDirAbsPath, ((JTextField) swingEngine.find("hyd_DSS_Init")).getText());

		// Copy DSS files to "Run" folder
		copyDSSFileToScenarioDirectory(runDirAbsPath, ((JTextField) swingEngine.find("hyd_DSS_SV")).getText());
		copyDSSFileToScenarioDirectory(runDirAbsPath, ((JTextField) swingEngine.find("hyd_DSS_Init")).getText());

		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT, "Saving the table files.");
		writeToFileIndexAndOption(swingEngine, guiLinks2BOList, runDirAbsPath + "//Lookup//",
				generatedDirAbsPath + "//Lookup//");
		writeUserDefinedTables(guiLinks2BOList, runDirAbsPath + "//Lookup//", generatedDirAbsPath + "//Lookup//");
		// Copying demand tables.
		String demandDirPath = "";
		if(((JRadioButton) swingEngine.find("dem_rdbCurSWP")).isSelected())
		{
			demandDirPath = Constant.MODEL_W2_WRESL_LOOKUP_DIR + "//VariableDemand//";
		}
		else
		{
			demandDirPath = Constant.MODEL_W2_WRESL_LOOKUP_DIR + "//FutureDemand//";
		}
		String lookupFilePath = "";
		try
		{
			// copy either variableDemand or futureDemand lookup tables to
			// "Generated" folder
			lookupFilePath = generatedDirAbsPath + "//Lookup//";
			FileUtils.copyDirectory(new File(demandDirPath), new File(lookupFilePath));
			// copy either variableDemand or futureDemand lookup tables to "Run"
			// folder
			lookupFilePath = runDirAbsPath + "//Lookup//";
			FileUtils.copyDirectory(new File(demandDirPath), new File(lookupFilePath));
		}
		catch(IOException ex)
		{
			throw new CalLiteGUIException(
					"There is a error when copying the directory from " + demandDirPath + " to " + lookupFilePath, ex);
		}

		// Copying WRIMSv2 DLL.
		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT, "Copying WRIMSv2 DLL.");
		// wrims2 ANN file name is different from wrims1
		String wrims2AnnSource;
		String wrims2AnnRun;
		String wrims2AnnGenerated;

		JRadioButton rdbSLR45 = (JRadioButton) swingEngine.find("hyd_rdb1");
		JRadioButton rdbSLR15 = (JRadioButton) swingEngine.find("hyd_rdb2");

		if(rdbSLR45.isSelected())
		{
			wrims2AnnSource = Constant.MODEL_W2_WRESL_DIR + "//External//Ann7inp_BDCP_LLT_45cm.dll";
			wrims2AnnRun = runDirAbsPath + "//External//Ann7inp_CA.dll";
			wrims2AnnGenerated = generatedDirAbsPath + "//External//Ann7inp_BDCP_LLT_45cm.dll";
		}
		else if(rdbSLR15.isSelected())
		{
			wrims2AnnSource = Constant.MODEL_W2_WRESL_DIR + "//External//Ann7inp_BDCP_ELT_15cm.dll";
			wrims2AnnRun = runDirAbsPath + "//External//Ann7inp_CA.dll";
			wrims2AnnGenerated = generatedDirAbsPath + "//External//Ann7inp_BDCP_ELT_15cm.dll";
		}
		else
		{
			wrims2AnnSource = Constant.MODEL_W2_WRESL_DIR + "//External//Ann7inp_BST_noSLR_111709.dll";
			wrims2AnnRun = runDirAbsPath + "//External//Ann7inp_CA.dll";
			wrims2AnnGenerated = generatedDirAbsPath + "//External//Ann7inp_BST_noSLR_111709.dll";
		}
		try
		{
			// copy dll to "Run" folder
			FileUtils.copyFile(Paths.get(wrims2AnnSource).toFile(), Paths.get(wrims2AnnRun).toFile());
		}
		catch(IOException ex)
		{
			throw new CalLiteGUIException("There is a error when copying the file from "
					+ Paths.get(wrims2AnnSource).toString() + " to " + Paths.get(wrims2AnnRun).toString(), ex);
		}
		try
		{
			// copy dll to "Generated" folder
			FileUtils.copyFile(Paths.get(wrims2AnnSource).toFile(), Paths.get(wrims2AnnGenerated).toFile());
		}
		catch(IOException ex)
		{
			throw new CalLiteGUIException("There is a error when copying the file from "
					+ Paths.get(wrims2AnnSource).toString() + " to " + Paths.get(wrims2AnnGenerated).toString(), ex);
		}

		// Creating study.sty.
		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT, "Creating study.sty");
		Calendar cal = Calendar.getInstance();

		String startMon = ((String) ((JSpinner) swingEngine.find("spnRunStartMonth")).getValue()).trim().toUpperCase();
		String endMon = ((String) ((JSpinner) swingEngine.find("spnRunEndMonth")).getValue()).trim().toUpperCase();
		Integer startYr = (Integer) ((JSpinner) swingEngine.find("spnRunStartYear")).getValue();
		Integer endYr = (Integer) ((JSpinner) swingEngine.find("spnRunEndYear")).getValue();

		// Determine Month/Count
		Integer dayct = getDaysinMonth(startMon);
		Integer iSMon = monthToInt(startMon);
		Integer iEMon = monthToInt(endMon);
		Integer numMon = (endYr - startYr) * 12 + (iEMon - iSMon) + 1;

		String oDSS = ((JTextField) swingEngine.find("run_txfoDSS")).getText().trim();

		String[] newtext = new String[20];
		Integer[] lineNum = new Integer[20];

		newtext[0] = fileName + Constant.CLS_EXT;
		lineNum[0] = 2;
		newtext[1] = cal.getTime().toString();
		lineNum[1] = 4;
		newtext[2] = runDirAbsPath;
		lineNum[2] = 7;
		newtext[3] = Paths.get(runDirAbsPath + "//CALLITE_BO_FUTURE.STY").toString();
		lineNum[3] = 8;
		newtext[4] = Paths.get(runDirAbsPath + "//MAIN.WRESL").toString();
		lineNum[4] = 9;
		if(oDSS.toUpperCase().endsWith(".DSS"))
		{
			newtext[6] = Paths.get(Constant.SCENARIOS_DIR + oDSS).toString();
			lineNum[6] = 11;
		}
		else
		{
			newtext[6] = Paths.get(Constant.SCENARIOS_DIR + oDSS + ".DSS").toString();
			lineNum[6] = 11;
		}

		lineNum[5] = 10;
		newtext[5] = Paths.get(runDirAbsPath + "\\DSS\\" + ((JTextField) swingEngine.find("hyd_DSS_SV")).getText())
						  .toString();
		lineNum[7] = 12;
		newtext[7] = Paths.get(runDirAbsPath + "\\DSS\\" + ((JTextField) swingEngine.find("hyd_DSS_Init")).getText())
						  .toString();

		newtext[8] = numMon.toString();
		lineNum[8] = 14;
		newtext[9] = dayct.toString();
		lineNum[9] = 15;
		newtext[10] = startMon;
		lineNum[10] = 16;
		newtext[11] = startYr.toString();
		lineNum[11] = 17;

		lineNum[12] = 33;
		newtext[12] = ((JTextField) swingEngine.find("hyd_DSS_SV_F")).getText();
		lineNum[13] = 34;
		newtext[13] = ((JTextField) swingEngine.find("hyd_DSS_Init_F")).getText();

		replaceLinesInFile(runDirAbsPath + "\\study.sty", lineNum, newtext);

		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT, "Writing WRIMSv2 Batchfile.");
		// configuration file for wrims v2
		Integer iStartMonth = TimeOperation.monthValue(startMon.toLowerCase());
		Integer iEndMonth = TimeOperation.monthValue(endMon.toLowerCase());
		Integer iStartDay = TimeOperation.numberOfDays(iStartMonth, startYr);
		Integer iEndDay = TimeOperation.numberOfDays(iEndMonth, endYr);

		Map<String, String> configMap = new HashMap<String, String>();
		configMap.put("MainFile", runDirAbsPath + "\\main.wresl");
		configMap.put("DvarFile", FilenameUtils.removeExtension(newtext[6]) + ".dss");
		configMap.put("SvarFile", newtext[5]);
		configMap.put("SvarFPart", newtext[12]);
		configMap.put("InitFile", newtext[7]);
		configMap.put("InitFPart", newtext[13]);
		configMap.put("StartYear", startYr.toString());
		configMap.put("StartMonth", iStartMonth.toString());
		configMap.put("StartDay", iStartDay.toString());
		configMap.put("EndYear", endYr.toString());
		configMap.put("EndMonth", iEndMonth.toString());
		configMap.put("EndDay", iEndDay.toString());
		configMap.put("UserPath", System.getProperty("user.dir"));
		configMap.put("ScenarioName", fileName);
		configMap.put("ScenarioPath", new File(runDirAbsPath).getParentFile().getAbsolutePath());
		configMap.put("RunPath", runDirAbsPath);
		configMap.put("ConfigFilePath",
				new File(configMap.get("ScenarioPath"), configMap.get("ScenarioName") + ".config").getAbsolutePath());
		configMap.put("ConfigFilePath_wsidi",
				new File(configMap.get("ScenarioPath"), configMap.get("ScenarioName") + "_wsidi.config")
						.getAbsolutePath());
		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT, "Writing Scenario Config.");
		// replace vars in config template file

		String configText = wrimsv2.wreslparser.elements.Tools
				.readFileAsString(Constant.MODEL_W2_DIR + "//config.template");

		configText = configText.replace("{SvarFile}", configMap.get("SvarFile"));
		configText = configText.replace("{SvarFPart}", configMap.get("SvarFPart"));
		configText = configText.replace("{InitFile}", configMap.get("InitFile"));
		configText = configText.replace("{InitFPart}", configMap.get("InitFPart"));
		// configText = configText.replace("{DvarFile}",
		// configMap.get("DvarFile"));
		configText = configText.replace("{StartYear}", configMap.get("StartYear"));
		configText = configText.replace("{StartMonth}", configMap.get("StartMonth"));
		configText = configText.replace("{EndYear}", configMap.get("EndYear"));
		configText = configText.replace("{EndMonth}", configMap.get("EndMonth"));
		configText = configText.replace("{StartDay}", configMap.get("StartDay"));
		configText = configText.replace("{EndDay}", configMap.get("EndDay"));

		// wsidi run config file
		String configTextWsidi = configText.replace("{MainFile}", "run\\main_wsidi.wresl");
		configTextWsidi = configTextWsidi.replace("{DvarFile}",
				FilenameUtils.getBaseName(configMap.get("DvarFile")) + "_wsidi.dss");

		try
		{
			File configFileWsidi = new File(configMap.get("ConfigFilePath_wsidi"));
			PrintWriter configFilePWWsidi = new PrintWriter(new BufferedWriter(new FileWriter(configFileWsidi)));
			configFilePWWsidi.print(configTextWsidi);
			configFilePWWsidi.flush();
			configFilePWWsidi.close();
			// normal run config file
			String configTextSimple = configText.replace("{MainFile}", "run\\main.wresl");
			configTextSimple = configTextSimple.replace("{DvarFile}", configMap.get("DvarFile"));
			File configFile = new File(configMap.get("ConfigFilePath"));
			PrintWriter configFilePW = new PrintWriter(new BufferedWriter(new FileWriter(configFile)));
			configFilePW.print(configTextSimple);
			configFilePW.flush();
			configFilePW.close();
		}
		catch(IOException ex)
		{
			throw new CalLiteGUIException("There is a error when building the congig file for wsidi", ex);
		}
		updateSaveStatusFile(runDirAbsPath + Constant.SAVE_FILE + Constant.TXT_EXT, "Save is completed.");
	}

	/**
	 * In this method we open the study.sty file and write the {@code newText}
	 * to the study.sty file.
	 *
	 * @param fileName The file name.
	 * @param lineNum  The line number.
	 * @param newText  The new text to replace.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private void replaceLinesInFile(String fileName, Integer[] lineNum, String[] newText) throws CalLiteGUIException
	{
		Integer lineCt = 0;
		Integer n = 0;
		StringBuffer sb = new StringBuffer();
		List<String> data = fileSystemSvc.getFileData(fileName, false);
		for(String textinLine : data)
		{
			lineCt = lineCt + 1;
			if(lineCt.equals(lineNum[n]))
			{
				sb.append(newText[n] + Constant.NEW_LINE);
				n = n + 1;
			}
			else
			{
				sb.append(textinLine + Constant.NEW_LINE);
			}
		}
		fileSystemSvc.saveDataToFile(fileName, sb.toString());
	}

	/**
	 * This will convert the string month into the int value.
	 *
	 * @param month The month name
	 * @return Will return the number of days for that month.
	 */
	private int getDaysinMonth(String month)
	{
		int dayct = 0;
		month = month.toLowerCase();
		switch(month)
		{
			case "jan":
			case "mar":
			case "may":
			case "jul":
			case "aug":
			case "oct":
			case "dec":
				dayct = 31;
				break;
			case "apr":
			case "jun":
			case "sep":
			case "nov":
				dayct = 30;
				break;
			case "feb":
				dayct = 28;
				break;
		}
		return dayct;
	}

	/**
	 * This will write the user define table data to the table files as given in
	 * the gui_link2.table
	 *
	 * @param guiLinks2BOList The data list from gui_link2.table.
	 * @param runDir          The string for the Run directory.
	 * @param generatedDir    The string for the Generated directory.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private void writeUserDefinedTables(List<GUILinks2BO> guiLinks2BOList, String runDir, String generatedDir)
			throws CalLiteGUIException
	{
		for(String tableName : this.userDefinedTableMap.keySet())
		{
			Map<String, StringBuilder> fileDataMap = new HashMap<>();
			DataTableModel table = this.userDefinedTableMap.get(tableName);
			if(tableName.equals("gui_xchanneldays") || tableName.equals("gui_EIRatio")
					|| tableName.equals("perc_UnimparedFlow") || tableName.equals(Constant.SWP_START_FILENAME)
					|| tableName.equals(Constant.CVP_START_FILENAME))
			{
				fileDataMap.put(tableName, saveTableLikeTable(tableName, table));
			}
			else if(tableName.equals("gui_EIsjr"))
			{
				fileDataMap.put(tableName, saveEisjrTable(tableName, table));
			}
			else if(tableName.equals("gui_x2active|gui_x2km"))
			{
				fileDataMap.putAll(saveX2Table(table));
			}
			else
			{
				fileDataMap.put(tableName, saveTableWithColumnNumber(tableName, table));
			}
			for(String fileName : fileDataMap.keySet())
			{
				fileSystemSvc.saveDataToFile(runDir + fileName + Constant.TABLE_EXT,
						fileDataMap.get(fileName).toString());
				fileSystemSvc.saveDataToFile(generatedDir + fileName + Constant.TABLE_EXT,
						fileDataMap.get(fileName).toString());
			}
		}
	}

	/**
	 * This method is used for the X2 table.
	 *
	 * @param table The table data
	 * @return Will return the map which holdes the tables data for X2.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private Map<String, StringBuilder> saveX2Table(DataTableModel table) throws CalLiteGUIException
	{
		Map<String, StringBuilder> map = new HashMap<>();
		Object[][] data = table.getData();
		Object[][] activeData = new Object[12][2];
		Object[][] kmData = new Object[12][6];
		for(int m = 0; m < data.length; m++)
		{
			for(int n = 0; n < 2; n++)
			{
				activeData[m][n] = data[m][n];
			}
		}
		for(int m = 0; m < data.length; m++)
		{
			if((boolean) activeData[m][1])
			{
				activeData[m][1] = 1;
			}
			else
			{
				activeData[m][1] = 0;
			}
		}
		for(int m = 0; m < kmData.length; m++)
		{
			kmData[m][0] = m + 1;
		}
		for(int m = 0; m < data.length; m++)
		{
			for(int n = 2; n < 7; n++)
			{
				kmData[m][n - 1] = data[m][n];
			}
		}
		map.put("gui_x2active", saveTableLikeTable("gui_x2active", new DataTableModel("", null, activeData, false)));
		map.put("gui_x2km", saveTableWithColumnNumber("gui_x2km", new DataTableModel("", null, kmData, false)));
		return map;
	}

	/**
	 * This is used for the Eisjr table only.
	 *
	 * @param tableName The table name.
	 * @param table     The data of the table.
	 * @return Will return the {@link StringBuilder} of the table data.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private StringBuilder saveEisjrTable(String tableName, DataTableModel table) throws CalLiteGUIException
	{
		StringBuilder fileDataStrBuff = getTheCommentFromFile(tableName);
		Object[][] tableData = table.getData();
		int offset = 1;
		int mul = 2;
		for(int colNum = 1; colNum < 6; colNum++)
		{
			for(int i = 0; i < tableData.length; i++)
			{
				fileDataStrBuff.append(i + 1).append(Constant.TAB_SPACE).append(colNum).append(
						Constant.TAB_SPACE).append(tableData[i][offset]).append(Constant.TAB_SPACE).append(
						tableData[i][mul]).append(Constant.NEW_LINE);
			}
			offset += 2;
			mul += 2;
		}
		return fileDataStrBuff;
	}

	/**
	 * This method will save the table like the bellow format.
	 *
	 * <pre>
	 * month	NDO	     SAC	 SJR
	 * 	 1	     0	      0	       0
	 * 	 2	     0	      0	       0
	 *   3       0  	  0        0
	 *   4	     0.75	  0	       0
	 *   5	     0.75	  0	       0.75
	 *   6	     0.75	  0	       0.75
	 *   7	     0.75	  0.75     0.75
	 *   8	     0.75	  0.75     0.75
	 *   9	     0.75	  0.75     0.75
	 *   10	     0	      0	       0
	 *   11      0	      0	       0
	 *   12	     0	      0        0
	 *
	 * </pre>
	 *
	 * @param tableName The table name.
	 * @param table     The data of the table.
	 * @return Will return the {@link StringBuilder} of the table data.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private StringBuilder saveTableLikeTable(String tableName, DataTableModel table) throws CalLiteGUIException
	{
		StringBuilder fileDataStrBuff = getTheCommentFromFile(tableName);
		Object[][] tableData = table.getData();
		for(final Object[] tableDatum : tableData)
		{
			for(final Object o : tableDatum)
			{
				fileDataStrBuff.append(o).append(Constant.TAB_SPACE);
			}
			fileDataStrBuff.append(Constant.NEW_LINE);
		}
		return fileDataStrBuff;
	}

	/**
	 * This method will save the table like the bellow format.
	 *
	 * <pre>
	 *
	 * month	Column Number	   value
	 *   1			1				10
	 *   2			1				54
	 *   3			1				98
	 *   4			1				45
	 *
	 * </pre>
	 *
	 * @param tableName The table name.
	 * @param table     The data of the table.
	 * @return Will return the {@link StringBuilder} of the table data.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private StringBuilder saveTableWithColumnNumber(String tableName, DataTableModel table) throws CalLiteGUIException
	{
		StringBuilder fileDataStrBuff = getTheCommentFromFile(tableName);
		Object[][] tableData = table.getData();
		for(int colNum = 1; colNum < tableData[0].length; colNum++)
		{
			for(int i = 0; i < tableData.length; i++)
			{
				fileDataStrBuff.append(i + 1).append(Constant.TAB_SPACE).append(colNum).append(
						Constant.TAB_SPACE).append(tableData[i][colNum]).append(Constant.NEW_LINE);
			}
		}
		return fileDataStrBuff;
	}

	/**
	 * This will get the comment lines in the old table file.
	 *
	 * @param tableName Table name with out existence.
	 * @return Will return the {@link StringBuilder} of the table data.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private StringBuilder getTheCommentFromFile(String tableName) throws CalLiteGUIException
	{
		StringBuilder fileDataStrBuff = new StringBuilder();
		String fileName = Paths.get(Constant.MODEL_W2_WRESL_LOOKUP_DIR + tableName + Constant.TABLE_EXT).toString();
		try
		{
			List<String> oldFileData = fileSystemSvc.getFileData(fileName, false);
			// Adding the headings of the default file.
			for(String line : oldFileData)
			{
				if(isDouble(line.split(Constant.TAB_OR_SPACE_DELIMITER)[0]))
				{
					break;
				}
				fileDataStrBuff.append(line + Constant.NEW_LINE);
			}
			return fileDataStrBuff;
		}
		catch(CalLiteGUIException ex)
		{
			throw new CalLiteGUIException(
					"There is a error when we are geting the comments from the file. The file path is " + fileName, ex);
		}
	}

	@Override
	public boolean isDouble(String value)
	{
		try
		{
			Double.parseDouble(value);
			return true;
		}
		catch(NumberFormatException ex)
		{
			return false;
		}
	}

	/**
	 * This method will write the index and option of the current state in the
	 * ui to there files given in the gui_link2.table.
	 *
	 * @param swingEngine     The object of the GUI.
	 * @param guiLinks2BOList The data list from gui_link2.table.
	 * @param runDir          The string for the Run directory.
	 * @param generatedDir    The string for the Generated directory.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private void writeToFileIndexAndOption(SwingEngine swingEngine, List<GUILinks2BO> guiLinks2BOList, String runDir,
										   String generatedDir) throws CalLiteGUIException
	{
		Map<String, List<GUILinks2BO>> tableNameMap = new HashMap<>();
		for(GUILinks2BO gUILinks2BO : guiLinks2BOList)
		{
			String tableName = gUILinks2BO.getTableName();
			if(tableName.equals(Constant.N_A))
			{
				continue;
			}
			if(tableNameMap.get(tableName) == null)
			{
				List<GUILinks2BO> data = new ArrayList<GUILinks2BO>();
				data.add(gUILinks2BO);
				tableNameMap.put(tableName, data);
			}
			else
			{
				tableNameMap.get(tableName).add(gUILinks2BO);
			}
		}
		for(String tableName : tableNameMap.keySet())
		{
			List<GUILinks2BO> data = tableNameMap.get(tableName);
			List<String> headerList = null;
			StringBuffer fileDataStrBuf = new StringBuffer();
			headerList = fileSystemSvc.getFileData(Constant.MODEL_W2_WRESL_LOOKUP_DIR + tableName, false,
					ScenarioSvcImpl::isComment);
			headerList.stream().forEach(header -> fileDataStrBuf.append(header + Constant.NEW_LINE));
			fileDataStrBuf.append(FilenameUtils.removeExtension(tableName) + Constant.NEW_LINE);
			fileDataStrBuf.append("Index" + Constant.OLD_DELIMITER + "Option" + Constant.NEW_LINE);
			for(GUILinks2BO gUILinks2BO : data)
			{
				String index = gUILinks2BO.getIndex();
				String option = gUILinks2BO.getOption();
				String description = Constant.EXCLAMATION + gUILinks2BO.getDescription();
				Component c = swingEngine.find(gUILinks2BO.getGuiId().trim());
				if(c instanceof JTextField || c instanceof NumericTextField || c instanceof JTextArea)
				{
					option = ((JTextComponent) c).getText();
					if(!(c instanceof JTextArea) && option.isEmpty())
					{
						option = "0";
					}
					fileDataStrBuf.append(index).append(Constant.OLD_DELIMITER).append(option).append(
							Constant.OLD_DELIMITER).append(description).append(Constant.NEW_LINE);
				}
				else if(c instanceof JRadioButton)
				{
					if(gUILinks2BO.getGuiId().startsWith("hyd_ckb"))
					{
						boolean isSelected = ((AbstractButton) swingEngine.find("hyd_rdb2005")).isSelected()
								|| ((AbstractButton) swingEngine.find("hyd_rdb2030")).isSelected();
						if(isSelected)
						{
							option = "0";
						}
					}
					if(((AbstractButton) c).isSelected())
					{
						fileDataStrBuf.append(index).append(Constant.OLD_DELIMITER).append(option).append(
								Constant.OLD_DELIMITER).append(description).append(Constant.NEW_LINE);
					}
				}
				else if(c instanceof JCheckBox)
				{
					if(((AbstractButton) c).isSelected())
					{
						if(gUILinks2BO.getGuiId().startsWith("ckbReg") && !"n/a".equals(gUILinks2BO.getRegID()))
						{
							int rID = Integer.parseInt(gUILinks2BO.getRegID());
							option = String.valueOf(this.regulationoptions[rID]);
						}
						else
						{
							option = "1";
						}
					}
					else
					{
						String naFlag = gUILinks2BO.getNoregulation();
						if("1".equals(naFlag))
						{
							option = "NA";
						}
						else
						{
							option = "0";
						}
					}
					fileDataStrBuf.append(index).append(Constant.OLD_DELIMITER).append(option).append(
							Constant.OLD_DELIMITER).append(description).append(Constant.NEW_LINE);
				}
				else if(c == null)
				{
					// control not found we have this
					// scenario with "GUI_SJR.table" index
					// 2.
					option = "0";
					fileDataStrBuf.append(index).append(Constant.OLD_DELIMITER).append(option).append(
							Constant.OLD_DELIMITER).append(description).append(Constant.NEW_LINE);
				}
			}
			fileSystemSvc.saveDataToFile(generatedDir + tableName, fileDataStrBuf.toString());
			fileSystemSvc.saveDataToFile(runDir + tableName, fileDataStrBuf.toString());
		}
	}

	/**
	 * This method will copy the DSS files to {@code dssFileName} directory.
	 *
	 * @param directory   The directory to copy.
	 * @param dssFileName The dss file name.
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private void copyDSSFileToScenarioDirectory(String directory, String dssFileName) throws CalLiteGUIException
	{
		Path fDestination = Paths.get(directory, "//DSS//" + dssFileName);
		try
		{
			FileUtils.copyFile(Paths.get(Constant.MODEL_W2_DSS_DIR + dssFileName).toFile(), fDestination.toFile());
		}
		catch(IOException ex)
		{
			throw new CalLiteGUIException("There is a problem copying the DSS Files. The file name is "
					+ Paths.get(Constant.MODEL_W2_DSS_DIR + dssFileName).toString() + " and the destination is "
					+ Paths.get(directory, "//DSS//" + dssFileName).toString(), ex);
		}
	}

	/**
	 * This will copy the {@code sourceDirectory} Directory to
	 * {@code destinationDirectory} Directory.
	 *
	 * @param sourceDirectory      The source Directory
	 * @param destinationDirectory The destination Directory
	 * @throws CalLiteGUIException It throws a general exception.
	 */
	private void createDirAndCopyFiles(String sourceDirectory, String destinationDirectory) throws CalLiteGUIException
	{
		try
		{
			FileUtils.copyDirectory(new File(sourceDirectory), new File(destinationDirectory));
		}
		catch(IOException ex)
		{
			throw new CalLiteGUIException("There is a problem copying the Directorys. The source directory is "
					+ sourceDirectory + " and the destination directory is " + destinationDirectory, ex);
		}
	}

	@Override
	public void saveToCLSFile(String fileName, SwingEngine swingEngine, List<GUILinks2BO> guiLinks2BOList)
			throws CalLiteGUIException
	{
		List<String> panelNames = new ArrayList<>();
		JTabbedPane main = (JTabbedPane) swingEngine.find(Constant.MAIN_PANEL_NAME);
		main.getComponents();
		for(Component child : main.getComponents())
		{
			if(child instanceof XScrollPane)
			{
				for(Component child1 : ((Container) child).getComponents())
				{
					if(child1 instanceof JViewport)
					{
						for(Component child2 : ((Container) child1).getComponents())
						{
							if(child2 instanceof JPanel && child2.getName() != null)
							{
								panelNames.add(child2.getName());
							}
						}
					}
				}
			}
		}
		// TODO: Update if needed for new result dashboards
		List<String> resultTabNames = Arrays.asList("Custom", "externalPDF", "Reporting", "schematics");
		panelNames.removeAll(resultTabNames);
		StringBuilder sb = new StringBuilder();
		panelNames.forEach(panelName -> setControlValues(swingEngine.find(panelName), sb));

		sb.append("DATATABLEMODELS" + Constant.NEW_LINE);
		Set<String> keys = this.userDefinedTableMap.keySet();
		if(!keys.isEmpty())
		{
			Optional<GUILinks2BO> gUILinks2BO;
			for(String key : keys)
			{
				try
				{
					gUILinks2BO = guiLinks2BOList.stream().filter(
							seedData -> seedData.getDataTables().equals(key)).findFirst();
					gUILinks2BO.ifPresent(guiLinks2BO -> sb.append(
							convertTableToString(guiLinks2BO.getTableID(), this.userDefinedTableMap.get(key)))
														   .append(Constant.NEW_LINE));
				}
				catch(NoSuchElementException ex)
				{
					sb.append(convertTableToString(key, this.userDefinedTableMap.get(key)) + Constant.NEW_LINE);
				}
			}
		}
		sb.append("END DATATABLEMODELS" + Constant.NEW_LINE);

		sb.append("REGULATIONOPTIONS" + Constant.NEW_LINE);
		StringBuilder sRegFlags = new StringBuilder(String.valueOf(this.regulationoptions[0]));
		for(int i = 1; i < this.regulationoptions.length; i++)
		{
			sRegFlags.append(Constant.PIPELINE).append(this.regulationoptions[i]);
		}
		sb.append(sRegFlags + Constant.NEW_LINE);
		sb.append("END REGULATIONOPTIONS" + Constant.NEW_LINE);

		sb.append("WSIDILABEL" + Constant.NEW_LINE);
		JLabel l = (JLabel) swingEngine.find("op_WSIDI_Status");
		sb.append(l.getText() + Constant.NEW_LINE);
		sb.append("END WSIDILABEL" + Constant.NEW_LINE);

		fileSystemSvc.saveDataToFile(fileName, sb.toString());
	}

	/**
	 * This method will convert the {@link DataTableModel} object into the table
	 * string which is stored in the cls file.
	 *
	 * @param tableId        The table id.
	 * @param dataTableModel The data of the table.
	 * @return return the data as a string.
	 */
	private String convertTableToString(String tableId, DataTableModel dataTableModel)
	{
		StringBuilder tableStr = new StringBuilder(tableId + Constant.PIPELINE);
		Object[][] data = dataTableModel.getData();
		for(final Object[] datum : data)
		{
			tableStr.append(datum[0]);
			for(int j = 1; j < data[0].length; j++)
			{
				tableStr.append(Constant.COMMA).append(datum[j]);
			}
			tableStr.append(Constant.SEMICOLON);
		}
		return tableStr.toString();
	}

	/**
	 * This will convert the ui controls into the string buffer which will be
	 * return to the cls file.
	 *
	 * @param component    The component which control values we need to set.
	 * @param stringBuffer The data that need to be set.
	 */
	private void setControlValues(Component component, StringBuilder stringBuffer)
	{
		String compName = "";
		String value = "";
		boolean val;
		compName = component.getName();
		if(compName != null)
		{
			if(component instanceof JTextField || component instanceof NumericTextField
					|| component instanceof JTextArea)
			{
				value = ((JTextComponent) component).getText();
				stringBuffer.append(compName).append(Constant.PIPELINE).append(value).append(Constant.NEW_LINE);
			}
			else if(component instanceof JSpinner)
			{
				value = ((JSpinner) component).getValue().toString();
				stringBuffer.append(compName).append(Constant.PIPELINE).append(value).append(Constant.NEW_LINE);
			}
			else if(component instanceof JCheckBox || component instanceof JRadioButton)
			{
				val = ((AbstractButton) component).isSelected();
				value = Boolean.toString(val);
				stringBuffer.append(compName).append(Constant.PIPELINE).append(value).append(Constant.NEW_LINE);
			}
		}
		for(Component child : ((Container) component).getComponents())
		{
			if(component instanceof JSpinner)
			{
				break;
			}
			if(child != null)
			{
				setControlValues(child, stringBuffer);
			}
		}
	}

	/**
	 * This method is used for the writing the state of the process to the file.
	 *
	 * @param statusFilename full path of the file name.
	 * @param text           text to write in the file.
	 */
	private void updateSaveStatusFile(String statusFilename, String text)
	{
		text += Constant.NEW_LINE;
		try
		{
			Files.write(Paths.get(statusFilename), text.getBytes(), StandardOpenOption.APPEND);
		}
		catch(NoSuchFileException ex)
		{
			try
			{
				Files.write(Paths.get(statusFilename), text.getBytes(), StandardOpenOption.CREATE);
			}
			catch(IOException e)
			{
				LOG.error(e.getMessage(), e);
			}
		}
		catch(IOException ex)
		{
			LOG.debug("IOException: " + ex.getMessage(), ex);
		}
	}
}

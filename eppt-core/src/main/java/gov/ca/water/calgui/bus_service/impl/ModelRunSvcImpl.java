/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bus_service.impl;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;
import javax.swing.*;

import gov.ca.water.calgui.bus_service.IModelRunSvc;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.commons.io.FilenameUtils;
import org.apache.log4j.Logger;

/**
 * This class will handle the batch run.
 *
 * @author Mohan
 */
public final class ModelRunSvcImpl implements IModelRunSvc
{

	private static final Logger LOG = Logger.getLogger(ModelRunSvcImpl.class.getName());
	private static int SIMULTANEOUS_RUNS = Math.max(1, Runtime.getRuntime().availableProcessors());
	private Properties _properties = new Properties();
	private static IErrorHandlingSvc _errorHandlingSvc = new ErrorHandlingSvcImpl();
	private int _wsdiIterations;
	private IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();

	public ModelRunSvcImpl()
	{
		try
		{
			_properties.load(ModelRunSvcImpl.class.getClassLoader().getResourceAsStream("callite-gui._properties"));
			_wsdiIterations = Integer.parseInt(_properties.getProperty("wsidi.iterations"));
		}
		catch(Exception ex)
		{
			LOG.error("Problem loading _properties. " + ex.getMessage(), ex);
		}
	}

	public static int getSimultaneousRuns()
	{
		return SIMULTANEOUS_RUNS;
	}

	public static void setSimultaneousRuns(int simultaneousRuns)
	{
		SIMULTANEOUS_RUNS = simultaneousRuns;
	}

	/**
	 * This will generate the main .bat file for WSI-DI.
	 *
	 * @param batFileName      The batch file name. If it is empty or null it will take the
	 *                         default batch file name.
	 * @param scenarioFileName The list of scenario names in this run.
	 * @param iterations       The number of iterations.
	 */
	private static void setupMainBatchFileWSIDI(String batFileName, Path scenarioFileName, final int iterations)
	{

		if(batFileName == null || batFileName.isEmpty())
		{
			batFileName = "CalLite_w2.bat";
		}
		String del = "";
		String scenarioName = FilenameUtils.removeExtension(scenarioFileName.getFileName().toString());
		String scenarioPath = scenarioFileName.toAbsolutePath().toString();
		String progressFilePath = new File(scenarioPath, "run\\progress.txt").getAbsolutePath();
		String wreslCheckFilePath = new File(scenarioPath, "run\\\"=WreslCheck_main=.log\"").getAbsolutePath();
		String wreslCheckWsidiFilePath = new File(scenarioPath, "run\\\"=WreslCheck_main_wsidi=.log\"")
				.getAbsolutePath();
		String wsidiIterationLogPath = new File(scenarioPath, "run\\wsidi_iteration.log").getAbsolutePath();
		del = del + "del /F /Q " + progressFilePath + "\r\n";
		del = del + "del /F /Q " + wreslCheckFilePath + "\r\n";
		del = del + "del /F /Q " + wreslCheckWsidiFilePath + "\r\n";
		del = del + "del /F /Q " + wsidiIterationLogPath + "\r\n";
		File batchFile = null;
		batchFile = new File(System.getProperty("user.dir"), batFileName);
		PrintWriter batchFilePW;
		String cmd = "Model_w2\\vscript.bat Model_w2\\vscript\\Main.py " + "\""
				+ new File(scenarioPath, scenarioName + "_wsidi.config").getAbsolutePath() + "\" " + iterations;
		try
		{
			batchFilePW = new PrintWriter(new BufferedWriter(new FileWriter(batchFile)));
			batchFilePW.println("@title=CalLiteRun" + scenarioName);
			batchFilePW.println(del);
			batchFilePW.println();
			batchFilePW.println(cmd);
			batchFilePW.flush();
			batchFilePW.close();
		}
		catch(IOException e)
		{
			LOG.debug(e);
		}
	}

	/**
	 * This method will run the batch program.
	 */
	private static void runBatch()
	{
		try
		{
			Runtime rt = Runtime.getRuntime();
			Process proc = rt
					.exec("cmd /c start /min \"CalLiteRun\" " + System.getProperty("user.dir") + "\\CalLite_w2.bat");
			int exitVal = proc.waitFor();
			LOG.debug("Return from batch run " + exitVal);
		}
		catch(Throwable t)
		{
			_errorHandlingSvc.businessErrorHandler("Run failure!", t);
			LOG.debug(t.getStackTrace());
		}
	}

	/**
	 * Convert the month name to the int value of it.
	 *
	 * @param month The month name which you want to convert.
	 * @return The int value of the month.
	 */
	public static int monthToInt(String month)
	{
		HashMap<String, Integer> monthMap = new HashMap<>();
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

	@Override
	public void doBatch(List<Path> scenarioNamesList, boolean isWsidi)
	{
		if(scenarioNamesList.isEmpty() || scenarioNamesList == null)
		{
			_errorHandlingSvc.validationeErrorHandler("The scenario name list is empty to run the batch program.",
					"The scenario name list is empty to run the batch program.");
		}
		// delete previous generated batch file
		deleteBatchFile();
		if(isWsidi)
		{
			if(scenarioNamesList.size() > 1)
			{
				_dialogSvc.getOK("WSIDI generation only allowed for a single hydroclimate realization.",
						JOptionPane.ERROR_MESSAGE);
			}
			else
			{
				setupMainBatchFileWSIDI(null, scenarioNamesList.get(0), _wsdiIterations);
				runBatch();
			}
		}
		else
		{
			// how many simultaneous run?
			int numberOfSimultaneousRun = SIMULTANEOUS_RUNS;
			// how many sub batch files?
			int numberOfSubBatch;
			if(numberOfSimultaneousRun == 0)
			{
				numberOfSubBatch = 1;
			}
			else
			{
				numberOfSubBatch = (int) Math.ceil((double) scenarioNamesList.size() / numberOfSimultaneousRun);
			}
			// sub batch file name array
			String[] subBatchFileNameArray = new String[numberOfSubBatch];
			for(int j = 0; j < numberOfSubBatch; j++)
			{
				subBatchFileNameArray[j] = "group_" + j + ".bat";
				File subBatchFile = new File(System.getProperty("user.dir"), subBatchFileNameArray[j]);
				deleteDirectory(subBatchFile);
				List<Path> groupScenFileNameList = new ArrayList<>();
				for(int i = j * numberOfSimultaneousRun; i < Math.min((j + 1) * numberOfSimultaneousRun,
						scenarioNamesList.size()); i++)
				{
					Path scenFileName = scenarioNamesList.get(i);
					groupScenFileNameList.add(scenFileName);
				}
				setupBatchFile(subBatchFileNameArray[j], groupScenFileNameList);
			}
			// generate main batch file
			setupMainBatchFile(null, scenarioNamesList, subBatchFileNameArray);
			// run all scenarios with 3 secs delay between jvm initialization
			runBatch();
		}
	}

	/**
	 * This will generate the main .bat file depending on the values passed in.
	 *
	 * @param batFileName           The batch file name. If it is empty or null it will take the
	 *                              default batch file name.
	 * @param scenarioNamesList     The list of scenario names in this run.
	 * @param subBatchFileNameArray The individual batch file name.
	 */
	public void setupMainBatchFile(String batFileName, List<Path> scenarioNamesList, String[] subBatchFileNameArray)
	{
		if(batFileName == null || batFileName.isEmpty())
		{
			batFileName = "CalLite_w2.bat";
		}
		StringBuilder del = new StringBuilder();
		for(Path scenarioName : scenarioNamesList)
		{
			String scenarioPath = new File(Constant.RUN_DETAILS_DIR + scenarioName).getAbsolutePath();
			String wsidilogFilePath = new File(scenarioPath, "run\\wsidi_iteration.log").getAbsolutePath();
			String progressFilePath = new File(scenarioPath, "run\\progress.txt").getAbsolutePath();
			String wreslCheckFilePath = new File(scenarioPath, "run\\\"=WreslCheck_main=.log\"").getAbsolutePath();
			String wreslCheckWsidiFilePath = new File(scenarioPath, "run\\\"=WreslCheck_main_wsidi=.log\"")
					.getAbsolutePath();
			del.append("del /F /Q ").append(wsidilogFilePath).append("\r\n");
			del.append("del /F /Q ").append(progressFilePath).append("\r\n");
			del.append("del /F /Q ").append(wreslCheckFilePath).append("\r\n");
			del.append("del /F /Q ").append(wreslCheckWsidiFilePath).append("\r\n");
		}
		File batchFile = null;
		batchFile = new File(System.getProperty("user.dir"), batFileName);
		PrintWriter batchFilePW;
		try
		{
			batchFilePW = new PrintWriter(new BufferedWriter(new FileWriter(batchFile)));
			batchFilePW.println(del);
			for(String subBat : subBatchFileNameArray)
			{
				batchFilePW.println("timeout /T 3");
				batchFilePW.println("start /wait /min " + subBat);
				batchFilePW.println();
			}
			batchFilePW.println("exit\n");
			batchFilePW.flush();
			batchFilePW.close();
		}
		catch(IOException e)
		{
			LOG.debug(e);
		}
	}

	/**
	 * This will generate the main .bat file for single run.
	 *
	 * @param batFileName      The batch file name
	 * @param scenarioFileName The scenario file name
	 */
	private void setupBatchFile(String batFileName, List<Path> scenarioFileName)
	{
		if(batFileName == null || batFileName.isEmpty())
		{
			batFileName = "CalLite_w2.bat";
		}
		File batchFile = null;
		batchFile = new File(System.getProperty("user.dir"), batFileName);

		try(FileWriter fileWriter = new FileWriter(batchFile);
			BufferedWriter bufferedWriter = new BufferedWriter(fileWriter);
			PrintWriter batchFilePW = new PrintWriter(bufferedWriter))
		{
			for(int i = 0; i < scenarioFileName.size(); i++)
			{
				String scenarioName = FilenameUtils.removeExtension(scenarioFileName.get(i).getFileName().toString());
				String scenarioPath = scenarioFileName.get(i).toAbsolutePath().toString();
				String configFilePath = new File(scenarioPath, scenarioName + ".config").getAbsolutePath();
				String batchText = "%~dp0\\Model_w2\\runConfig_calgui " + configFilePath + " " + scenarioName;
				if(i < scenarioFileName.size() - 1)
				{
					batchFilePW.println("start /min " + batchText);
					batchFilePW.println("timeout 20");
				}
				else
				{
					batchFilePW.println("@title = \"" + batchText + "\"");
					batchFilePW.println(batchText);
					batchFilePW.println();
				}
			}
			batchFilePW.flush();
		}
		catch(IOException e)
		{
			LOG.debug(e);
		}
	}

	/**
	 * This method will delete the whole directory.
	 *
	 * @param directory The directory name.
	 * @return This will return whether it is successful or not.
	 */
	public boolean deleteDirectory(File directory)
	{
		if(directory.isDirectory())
		{
			String[] children = directory.list();
			if(children != null)
			{
				for(int i = 0; i < children.length; i++)
				{
					boolean success = deleteDirectory(new File(directory, children[i]));
					if(!success)
					{
						return false;
					}
				}
			}
		}
		return directory.delete();
	}

	/**
	 * This will delete the default batch file.
	 */
	private void deleteBatchFile()
	{
		File batchFile = new File(System.getProperty("user.dir"), "CalLite_w2.bat");
		boolean result = batchFile.delete();
		if (!result)
		{
			LOG.error("Unable to delete batch file: " + batchFile.getAbsolutePath());
		}
	}
}

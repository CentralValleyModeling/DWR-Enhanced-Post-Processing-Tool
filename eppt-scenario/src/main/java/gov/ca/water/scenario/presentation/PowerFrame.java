/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.scenario.presentation;

import java.awt.Dimension;
import java.awt.HeadlessException;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.busservice.IDSSGrabber1Svc;
import gov.ca.water.calgui.busservice.impl.DSSGrabber1SvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.techservice.IDialogSvc;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

class PowerFrame
{
	private static final Logger LOG = Logger.getLogger(PowerFrame.class.getName());
	private IErrorHandlingSvc _errorHandlingSvc = new ErrorHandlingSvcImpl();
	private IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();

	PowerFrame(List<RBListItemBO> lstScenarios)
	{
		try
		{
			String dssFilename = "";
			IDSSGrabber1Svc dssGrabber = new DSSGrabber1SvcImpl();
			for(RBListItemBO item : lstScenarios)
			{
				if(item.isSelected())
				{
					dssFilename = item.toString();
//					dssGrabber.setScenarioRuns(item.toString(), item.getModel());
				}
			}
			if(!dssGrabber.hasPower(dssFilename))
			{
				if("OK".equals(_dialogSvc.getOKCancel(
						"There are no power records in " + dssGrabber.getBaseRunName() + ". Press OK to run the Power Module.",
						JOptionPane.PLAIN_MESSAGE)))
				{
					runPowerModule("DEFAULT");
				}
			}
			else
			{
				final JFrame frame = new JFrame("Power Viewer:" + dssGrabber.getBaseRunName());
				frame.setPreferredSize(new Dimension(800, 600));
				frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
				frame.pack();
				frame.setVisible(true);
			}
		}
		catch(HeadlessException e)
		{
			LOG.error(e.getMessage());
			String messageText = "Unable to display Power frame.";
			_errorHandlingSvc.businessErrorHandler(messageText, e);
		}
	}

	private boolean runPowerModule(String scenarioName)
	{

		boolean success = false;

		String scenarioPath = new File(Constant.RUN_DETAILS_DIR + scenarioName).getAbsolutePath();
		String progressFilePath = new File(scenarioPath, "run\\progress.txt").getAbsolutePath();

		File batchFile = new File(System.getProperty("user.dir"), "CalLite_w2.bat");
		PrintWriter batchFilePW;
		try
		{
			batchFilePW = new PrintWriter(new BufferedWriter(new FileWriter(batchFile)));
			batchFilePW.println("del /F /Q " + progressFilePath);
			batchFilePW.println("start /wait group_0.bat");
			batchFilePW.println();
			batchFilePW.flush();
			batchFilePW.close();
		}
		catch(IOException e)
		{
			LOG.debug("Unable to create group_0.bat");
		}

		File groupBatchFile = new File(System.getProperty("user.dir"), "group_0.bat");
		try
		{
			batchFilePW = new PrintWriter(new BufferedWriter(new FileWriter(groupBatchFile)));
			batchFilePW.println("echo Power Module Executing > " + progressFilePath);
			batchFilePW.println("pause");
			batchFilePW.println("echo Power Module Done > " + progressFilePath);
			batchFilePW.println("exit");
			batchFilePW.flush();
			batchFilePW.close();
		}
		catch(IOException e)
		{
			LOG.debug("Unable to create group_0.bat");
		}

		try
		{
			Runtime rt = Runtime.getRuntime();
			Process proc = rt
					.exec("cmd /c start /min \"CalLiteRun\" " + System.getProperty("user.dir") + "\\CalLite_w2.bat");
			int exitVal = proc.waitFor();
			success = (exitVal == 0);
			LOG.debug("Return from batch run " + exitVal);
		}
		catch(Throwable t)
		{
			_errorHandlingSvc.businessErrorHandler("Run failure!", "Power Generation module did not run.");
			LOG.debug(t.getStackTrace());
		}
		return success;
	}

}

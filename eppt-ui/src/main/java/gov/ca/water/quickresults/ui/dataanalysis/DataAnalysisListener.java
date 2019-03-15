/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.dataanalysis;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;
import java.util.concurrent.CompletableFuture;
import javax.swing.*;

import gov.ca.water.calgui.bo.FileDialogBO;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.presentation.Report;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-27-2019
 */
public class DataAnalysisListener implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(DataAnalysisListener.class.getName());
	private static final String INP_FILE_EXTENSION = "inp";
	private static final String PDF_FILE_EXTENSION = "PDF";
	private final DataAnalysisPanel _dataAnalysisPanel;

	public DataAnalysisListener(DataAnalysisPanel dataAnalysisPanel)
	{
		_dataAnalysisPanel = dataAnalysisPanel;
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		Object source = e.getSource();
		if(source instanceof Component)
		{
			Component component = ((Component) source);
			if(component.getName() != null)
			{
				switch(component.getName())
				{
					case "btnGetTemplateFile":
						selectTemplateFile(e);
						break;
					case "btnGetReportFile1":
						selectDssFile(e, _dataAnalysisPanel.getDssResultFileField1());
						break;
					case "btnGetReportFile2":
						selectDssFile(e, _dataAnalysisPanel.getDssResultFileField2());
						break;
					case "btnGetReportFile3":
						selectOutputFile(e);
						break;
					case "btnReport":
						generateReportAction();
						break;
					default:
						LOGGER.info("Action Command " + e.getActionCommand());
						LOGGER.info("Action Source " + component.getName());
				}
			}
		}
	}

	private JFrame getMainWindow()
	{
		Window window = SwingUtilities.windowForComponent(_dataAnalysisPanel);
		return (JFrame) window;
	}

	private void selectDssFile(ActionEvent e, JTextField jTextField)
	{
		FileDialogBO fileDialogBO = new FileDialogBO(jTextField, _dataAnalysisPanel);
		fileDialogBO.actionPerformed(e);
	}

	private void selectTemplateFile(ActionEvent e)
	{
		FileDialogBO fileDialogBO = new FileDialogBO(_dataAnalysisPanel.getReportTemplateTextField(),
				INP_FILE_EXTENSION, _dataAnalysisPanel);
		fileDialogBO.actionPerformed(e);
	}

	private void selectOutputFile(ActionEvent e)
	{
		FileDialogBO fileDialogBO = new FileDialogBO(_dataAnalysisPanel.getOutputTextField(), PDF_FILE_EXTENSION,
				_dataAnalysisPanel);
		fileDialogBO.actionPerformed(e);
	}

	/**
	 * Generates PDF report from "External PDF" dashboard - action "ACGenReport"
	 */
	private void generateReportAction()
	{
		String errorMsg = isInputsValid();
		if(errorMsg != null)
		{
			IDialogSvc dialogSvcInstance = DialogSvcImpl.getDialogSvcInstance();
			dialogSvcInstance.getOK(errorMsg, JOptionPane.ERROR_MESSAGE);
		}
		else
		{
			String filePath = EpptPreferences.getReportsPath().resolve(_dataAnalysisPanel.getOutputTextField().getText()).toString();
			File outputFile = new File(filePath);
			if(outputFile.exists())
			{
				IDialogSvc dialogSvcInstance = DialogSvcImpl.getDialogSvcInstance();
				String message = "The output file: '" + outputFile.getAbsolutePath() + "' already exists."
						+ "\n Would you like to continue and replace the existing file?";
				String yesNo = dialogSvcInstance.getYesNo(message, JOptionPane.WARNING_MESSAGE);
				if("Yes".equalsIgnoreCase(yesNo))
				{
					int i = 0;
				}
				else
				{
					return;
				}
			}
			generateReport();
		}
	}



	private String isInputsValid()
	{
		String errorMsg = null;
		if(_dataAnalysisPanel.getDssResultFileField1().getText().isEmpty())
		{
			return "You must specify DSS result file #1";
		}
		else if(_dataAnalysisPanel.getDssResultFileField2().getText().isEmpty())
		{
			return "You must specify DSS result file #2";

		}
		else if(_dataAnalysisPanel.getOutputTextField().getText().isEmpty())
		{
			return "You must specify the report output PDF file";
		}

		//check to see if the selected output path is a valid file name and path
		String filePath = EpptPreferences.getReportsPath().resolve(_dataAnalysisPanel.getOutputTextField().getText()).toString();
		File outputFile = new File(filePath);
		if(!outputFile.exists())
		{
			try
			{
				outputFile.createNewFile();
				outputFile.delete();
			}
			catch (IOException ex)
			{
				return "Error creating the Report Output File: " + outputFile.getName() + "\n" + ex.getMessage();
			}
		}

		String fontSize = _dataAnalysisPanel.getReportSize().getText();
		if(fontSize == null || fontSize.isEmpty())
		{
			return "You must specify a font size";
		}

		try
		{
			double d = Double.parseDouble(fontSize);
			if(d%1 != 0)
			{
				errorMsg = "The font size has to be an integer value";
			}
			else if(d<0)
			{
				errorMsg = "The font size cannot be negative";
			}
			else if(d==0)
			{
				errorMsg = "The font size cannot be zero";
			}
		}
		catch(Exception ex)
		{
			errorMsg = "The font size entered could not be converted to a number";
		}
		return errorMsg;
	}

	private void generateReport()
	{
		String fileName = _dataAnalysisPanel.getReportTemplateTextField().getToolTipText();
		try(FileInputStream fin = new FileInputStream(fileName);
			BufferedReader br = new BufferedReader(new InputStreamReader(fin)))
		{
			// Open the template file
			StringBuilder theText = new StringBuilder(br.readLine() + "\n");
			theText.append(br.readLine()).append("\n");
			theText.append(br.readLine()).append("\n");
			String skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText.append("FILE_BASE\t").append(_dataAnalysisPanel.getDssResultFileField1().getToolTipText()).append(
					"\n");
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText.append("NAME_BASE\t\"").append(_dataAnalysisPanel.getReportName1().getText()).append("\"\n");
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText.append("FILE_ALT\t").append(_dataAnalysisPanel.getDssResultFileField2().getToolTipText()).append(
					"\n");
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText.append("NAME_ALT\t\"").append(_dataAnalysisPanel.getReportName2().getText()).append("\"\n");
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText.append("OUTFILE\t").append(_dataAnalysisPanel.getOutputTextField().getToolTipText()).append("\n");
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText.append("NOTE\t\"").append(_dataAnalysisPanel.getReportNotes().getText()).append("\"\n");
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText.append("ASSUMPTIONS\t\"").append(_dataAnalysisPanel.getReportAssumptions().getText()).append(
					"\"\n");
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText.append("MODELER\t\"").append(_dataAnalysisPanel.getReportModeler().getText()).append("\"\n");

			theText.append("TABLE_FONT_SIZE\t").append(_dataAnalysisPanel.getReportSize().getText()).append("\n");

			String aLine = br.readLine();
			while(aLine != null)
			{
				theText.append(aLine).append("\n");
				aLine = br.readLine();
			}
			theText.append("\n");
			ByteArrayInputStream bs = new ByteArrayInputStream(theText.toString().getBytes());
			Report report = new Report(bs, _dataAnalysisPanel.getOutputTextField().getToolTipText(), getMainWindow());
			_dataAnalysisPanel.getReportButton().setEnabled(false);
			CompletableFuture.runAsync(report)
							 .thenRunAsync(() -> _dataAnalysisPanel.getReportButton().setEnabled(true),
									 SwingUtilities::invokeLater);
		}
		catch(IOException e1)
		{
			LOGGER.error(e1.getMessage());
			String messageText = "Unable to display report.";
			ErrorHandlingSvcImpl errorHandlingSvc = new ErrorHandlingSvcImpl();
			errorHandlingSvc.businessErrorHandler(messageText, e1);
		}
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.CompletableFuture;
import javax.swing.*;

import gov.ca.water.calgui.bo.FileDialogBO;
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
class DataAnalysisListener implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(DataAnalysisListener.class.getName());
	private static final String INP_FILE_EXTENSION = "inp";
	private static final String PDF_FILE_EXTENSION = "PDF";
	private final DataAnalysisPanel _dataAnalysisPanel;

	DataAnalysisListener(DataAnalysisPanel dataAnalysisPanel)
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
				{
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
		FileDialogBO fileDialogBO = new FileDialogBO(null, jTextField, getMainWindow());
		fileDialogBO.actionPerformed(e);
	}

	private void selectTemplateFile(ActionEvent e)
	{
		FileDialogBO fileDialogBO = new FileDialogBO(null, _dataAnalysisPanel.getReportTemplateTextField(),
				INP_FILE_EXTENSION, getMainWindow());
		fileDialogBO.actionPerformed(e);
	}

	private void selectOutputFile(ActionEvent e)
	{
		FileDialogBO fileDialogBO = new FileDialogBO(null, _dataAnalysisPanel.getOutputTextField(), PDF_FILE_EXTENSION,
				getMainWindow());
		fileDialogBO.actionPerformed(e);
	}

	/**
	 * Generates PDF report from "External PDF" dashboard - action "ACGenReport"
	 */
	private void generateReportAction()
	{
		if(_dataAnalysisPanel.getDssResultFileField1().getText().isEmpty()
				|| _dataAnalysisPanel.getDssResultFileField2().getText().isEmpty()
				|| _dataAnalysisPanel.getOutputTextField().getText().isEmpty())
		{
			IDialogSvc dialogSvcInstance = DialogSvcImpl.getDialogSvcInstance();
			dialogSvcInstance.getOK("You must specify the source DSS files and the output PDF file",
					JOptionPane.ERROR_MESSAGE);
		}
		else
		{
			generateReport();
		}
	}

	private void generateReport()
	{
		try(FileInputStream fin = new FileInputStream(
				_dataAnalysisPanel.getReportTemplateTextField().getToolTipText());
			BufferedReader br = new BufferedReader(new InputStreamReader(fin)))
		{

			// Create an inputstream from template file;
			// Open the template file
			String theText = br.readLine() + "\n";
			theText = theText + br.readLine() + "\n";
			theText = theText + br.readLine() + "\n";
			String skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText = theText + "FILE_BASE\t" + _dataAnalysisPanel.getDssResultFileField1().getToolTipText()
					+ "\n";
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText = theText + "NAME_BASE\t\"" + _dataAnalysisPanel.getReportName1().getText()
					+ "\"\n";
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText = theText + "FILE_ALT\t" + _dataAnalysisPanel.getDssResultFileField2().getToolTipText()
					+ "\n";
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText = theText + "NAME_ALT\t\"" + _dataAnalysisPanel.getReportName2().getText()
					+ "\"\n";
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText = theText + "OUTFILE\t" + _dataAnalysisPanel.getOutputTextField().getToolTipText()
					+ "\n";
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText = theText + "NOTE\t\"" + _dataAnalysisPanel.getReportNotes().getText() + "\"\n";
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText = theText + "ASSUMPTIONS\t\"" + _dataAnalysisPanel.getReportAssumptions().getText()
					+ "\"\n";
			skipLine = br.readLine();
			LOGGER.debug("Skip Line: " + skipLine);
			theText = theText + "MODELER\t\"" + _dataAnalysisPanel.getReportModeler().getText()
					+ "\"\n";

			theText = theText + "TABLE_FONT_SIZE\t" + _dataAnalysisPanel.getReportSize().getText()
					+ "\n";

			String aLine = br.readLine();
			while(aLine != null)
			{
				theText = theText + aLine + "\n";
				aLine = br.readLine();
			}
			theText = theText + "\n";
			ByteArrayInputStream bs = new ByteArrayInputStream(theText.getBytes());
			Report report = new Report(bs, _dataAnalysisPanel.getOutputTextField().getToolTipText());
			_dataAnalysisPanel.getReportButton().setEnabled(false);
			CompletableFuture.runAsync(report, SwingUtilities::invokeLater)
							 .thenRunAsync(() -> _dataAnalysisPanel.getReportButton().setEnabled(true),
									 SwingUtilities::invokeLater);
		}
		catch(IOException e1)
		{
			LOGGER.error(e1.getMessage());
			String messageText = "Unable to display report.";
			ErrorHandlingSvcImpl errorHandlingSvc = new ErrorHandlingSvcImpl();
			errorHandlingSvc.businessErrorHandler(messageText, getMainWindow(), e1);
		}
	}
}

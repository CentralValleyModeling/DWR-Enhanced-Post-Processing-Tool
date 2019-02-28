/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import java.awt.BorderLayout;
import java.awt.Container;
import javax.swing.*;

import org.apache.log4j.Logger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-26-2019
 */
public class DataAnalysisPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanel.class.getName());
	private static final String DATA_ANALYSIS_XML_PATH = "ui/Data_Analysis.xml";

	public DataAnalysisPanel()
	{
		try
		{
			super.setLayout(new BorderLayout());
			Container swixmlQuickResultsPanel = getSwingEngine().render(DATA_ANALYSIS_XML_PATH);
			super.add(swixmlQuickResultsPanel);
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + DATA_ANALYSIS_XML_PATH, e);
			throw new IllegalStateException(e);
		}
	}

	JTextField getReportTemplateTextField()
	{
		return (JTextField) getSwingEngine().find("tfTemplateFILE");
	}

	JTextField getDssResultFileField1()
	{
		return (JTextField) getSwingEngine().find("tfReportFILE1");
	}

	JTextField getDssResultFileField2()
	{
		return (JTextField) getSwingEngine().find("tfReportFILE2");
	}

	JTextField getOutputTextField()
	{
		return (JTextField) getSwingEngine().find("tfReportFILE3");
	}

	JButton getReportButton()
	{
		return (JButton) getSwingEngine().find("btnReport");
	}

	JTextField getReportName1()
	{
		return ((JTextField) getSwingEngine().find("tfReportNAME1"));
	}

	JTextField getReportName2()
	{
		return ((JTextField) getSwingEngine().find("tfReportNAME2"));
	}

	JTextArea getReportNotes()
	{
		return ((JTextArea) getSwingEngine().find("taReportNOTES"));
	}

	JTextArea getReportAssumptions()
	{
		return ((JTextArea) getSwingEngine().find("taReportASSUMPTIONS"));
	}

	JTextField getReportModeler()
	{
		return ((JTextField) getSwingEngine().find("tfReportMODELER"));
	}

	JTextField getReportSize()
	{
		return ((JTextField) getSwingEngine().find("tfFontSize"));
	}

	public void setActionListener(DataAnalysisListener dataAnalysisListener)
	{
		getSwingEngine().setActionListener(this, dataAnalysisListener);
	}
}

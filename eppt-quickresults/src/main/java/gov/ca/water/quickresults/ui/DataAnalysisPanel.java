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
import org.swixml.SwingEngine;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-26-2019
 */
public class DataAnalysisPanel extends JPanel
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanel.class.getName());
	private static final String DATA_ANALYSIS_XML_PATH = "ui/Data_Analysis.xml";
	private final SwingEngine _swingEngine;

	public DataAnalysisPanel()
	{
		_swingEngine = new SwingEngine();
		try
		{
			super.setLayout(new BorderLayout());
			Container swixmlQuickResultsPanel = _swingEngine.render(DATA_ANALYSIS_XML_PATH);
			super.add(swixmlQuickResultsPanel);
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + DATA_ANALYSIS_XML_PATH, e);
			throw new IllegalStateException(e);
		}
	}

	SwingEngine getSwingEngine()
	{
		return _swingEngine;
	}

	JTextField getReportTemplateTextField()
	{
		return (JTextField) _swingEngine.find("tfTemplateFILE");
	}

	JTextField getDssResultFileField1()
	{
		return (JTextField) _swingEngine.find("tfReportFILE1");
	}

	JTextField getDssResultFileField2()
	{
		return (JTextField) _swingEngine.find("tfReportFILE2");
	}

	JTextField getOutputTextField()
	{
		return (JTextField) _swingEngine.find("tfReportFILE3");
	}

	JButton getReportButton()
	{
		return (JButton) _swingEngine.find("btnReport");
	}

	JTextField getReportName1()
	{
		return ((JTextField) _swingEngine.find("tfReportNAME1"));
	}

	JTextField getReportName2()
	{
		return ((JTextField) _swingEngine.find("tfReportNAME2"));
	}

	JTextArea getReportNotes()
	{
		return ((JTextArea) _swingEngine.find("taReportNOTES"));
	}

	JTextArea getReportAssumptions()
	{
		return ((JTextArea) _swingEngine.find("taReportASSUMPTIONS"));
	}

	JTextField getReportModeler()
	{
		return ((JTextField) _swingEngine.find("tfReportMODELER"));
	}

	JTextField getReportSize()
	{
		return ((JTextField) _swingEngine.find("tfFontSize"));
	}
}

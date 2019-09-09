/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.quickresults.ui.dataanalysis;

import java.awt.BorderLayout;
import java.awt.Container;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.swing.*;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.quickresults.QuickResultsPanel;
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
	private static final String DATA_ANALYSIS_XML_FILE = "Data_Analysis.xml";

	public DataAnalysisPanel()
	{
		try
		{
			super.setLayout(new BorderLayout());
			Container swixmlQuickResultsPanel = renderSwixml(DATA_ANALYSIS_XML_FILE);
			super.add(swixmlQuickResultsPanel);
			JTextField tfTemplateFile = (JTextField) getSwingEngine().find("tfTemplateFILE");
			Path defaultTemplateFile = Paths.get(Constant.CONFIG_DIR).resolve(tfTemplateFile.getText());
			tfTemplateFile.setToolTipText(defaultTemplateFile.toString());
			JTextField tfReportFILE3 = (JTextField) getSwingEngine().find("tfReportFILE3");
			tfReportFILE3.setToolTipText(EpptPreferences.getReportsPath().resolve(tfReportFILE3.getText()).toString());
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + DATA_ANALYSIS_XML_FILE, e);
			throw new IllegalStateException(e);
		}
	}

	@Override
	public String getJavaHelpId()
	{
		return "Data Analysis";
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


}

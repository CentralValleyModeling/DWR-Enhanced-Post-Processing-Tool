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
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.swing.*;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.calgui.project.EpptConfigurationController;
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
	private final ButtonGroup _baseRadioGroup;
	private final ButtonGroup _altRadioGroup;

	public DataAnalysisPanel(EpptConfigurationController epptConfigurationController)
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
			_baseRadioGroup = new ButtonGroup();
			_altRadioGroup = new ButtonGroup();
			updateScenarios(epptConfigurationController.getEpptScenarioBase().orElse(null), epptConfigurationController.getEpptScenarioAlternatives());
			initRadioButtons();
			initListeners();
			epptConfigurationController.addScenarioChangedListener(this::updateScenarios);
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + DATA_ANALYSIS_XML_FILE, e);
			throw new IllegalStateException(e);
		}
	}

	private void initListeners()
	{
		JComboBox<EpptScenarioRun> baseScenarioCombo = (JComboBox<EpptScenarioRun>) getSwingEngine().find("baseScenarioCombo");
		JComboBox<EpptScenarioRun> altScenarioCombo = (JComboBox<EpptScenarioRun>) getSwingEngine().find("altScenarioCombo");
		baseScenarioCombo.addActionListener(e->
		{
			Object selectedItem = baseScenarioCombo.getSelectedItem();
			if(selectedItem != null)
			{
				((JTextField) getSwingEngine().find("tfReportNAME1")).setText(selectedItem.toString());
			}
		});
		altScenarioCombo.addActionListener(e->
		{
			Object selectedItem = altScenarioCombo.getSelectedItem();
			if(selectedItem != null)
			{
				((JTextField) getSwingEngine().find("tfReportNAME2")).setText(selectedItem.toString());
			}
		});
		JRadioButton baseScenarioRadioButton = (JRadioButton) getSwingEngine().find("baseScenarioBtn");
		JRadioButton baseFileRadioButton = (JRadioButton) getSwingEngine().find("baseFileBtn");
		JRadioButton altScenarioRadioButton = (JRadioButton) getSwingEngine().find("altScenarioBtn");
		JRadioButton altFileRadioButton = (JRadioButton) getSwingEngine().find("altFileBtn");
		baseScenarioRadioButton.addActionListener(e->baseScenarioCombo.setEnabled(baseScenarioRadioButton.isSelected()));
		baseFileRadioButton.addActionListener(e->baseScenarioCombo.setEnabled(!baseFileRadioButton.isSelected()));
		altScenarioRadioButton.addActionListener(e->altScenarioCombo.setEnabled(altScenarioRadioButton.isSelected()));
		altFileRadioButton.addActionListener(e->altScenarioCombo.setEnabled(!altFileRadioButton.isSelected()));
	}

	@SuppressWarnings("unchecked")
	private void updateScenarios(EpptScenarioRun base, List<EpptScenarioRun> alternatives)
	{
		List<EpptScenarioRun> scenarioRuns = new ArrayList<>();
		scenarioRuns.add(base);
		scenarioRuns.addAll(alternatives);
		scenarioRuns.removeIf(Objects::isNull);
		JComboBox<EpptScenarioRun> baseScenarioCombo = (JComboBox<EpptScenarioRun>) getSwingEngine().find("baseScenarioCombo");
		baseScenarioCombo.removeAllItems();
		scenarioRuns.stream().filter(Objects::nonNull).forEach(baseScenarioCombo::addItem);
		if(base != null)
		{
			baseScenarioCombo.setSelectedItem(base);
		}
		JComboBox<EpptScenarioRun> altScenarioCombo = (JComboBox<EpptScenarioRun>) getSwingEngine().find("altScenarioCombo");
		altScenarioCombo.removeAllItems();
		scenarioRuns.stream().filter(Objects::nonNull).forEach(altScenarioCombo::addItem);
		if(!alternatives.isEmpty())
		{
			altScenarioCombo.setSelectedItem(alternatives.get(0));
		}
	}

	private void initRadioButtons()
	{
		JRadioButton baseScenarioRadioButton = (JRadioButton) getSwingEngine().find("baseScenarioBtn");
		JRadioButton baseFileRadioButton = (JRadioButton) getSwingEngine().find("baseFileBtn");
		_baseRadioGroup.add(baseScenarioRadioButton);
		_baseRadioGroup.add(baseFileRadioButton);
		JRadioButton altScenarioRadioButton = (JRadioButton) getSwingEngine().find("altScenarioBtn");
		JRadioButton altFileRadioButton = (JRadioButton) getSwingEngine().find("altFileBtn");
		_altRadioGroup.add(altScenarioRadioButton);
		_altRadioGroup.add(altFileRadioButton);
	}

	@Override
	public String getJavaHelpId()
	{
		return "3.4_DataAnalysis.htm";
	}

	JTextField getReportTemplateTextField()
	{
		return (JTextField) getSwingEngine().find("tfTemplateFILE");
	}

	@SuppressWarnings("unchecked")
	Path getDssResultPathBase()
	{
		if(((JRadioButton) getSwingEngine().find("baseScenarioBtn")).isSelected())
		{
			JComboBox<EpptScenarioRun> baseScenarioCombo = (JComboBox<EpptScenarioRun>) getSwingEngine().find("baseScenarioCombo");
			Object selectedItem = baseScenarioCombo.getSelectedItem();
			Path path = Paths.get("");
			if(selectedItem instanceof EpptScenarioRun)
			{
				path = ((EpptScenarioRun) selectedItem).getDssContainer().getDvDssFile().getDssPath();
			}
			return path;
		}
		else
		{
			return Paths.get(((JTextField) getSwingEngine().find("tfReportFILE1")).getToolTipText());
		}
	}

	@SuppressWarnings("unchecked")
	Path getDssResultPathAlt()
	{
		if(((JRadioButton) getSwingEngine().find("altScenarioBtn")).isSelected())
		{
			JComboBox<EpptScenarioRun> altScenarioCombo = (JComboBox<EpptScenarioRun>) getSwingEngine().find("altScenarioCombo");
			Object selectedItem = altScenarioCombo.getSelectedItem();
			Path path = Paths.get("");
			if(selectedItem instanceof EpptScenarioRun)
			{
				path = ((EpptScenarioRun) selectedItem).getDssContainer().getDvDssFile().getDssPath();
			}
			return path;
		}
		else
		{
			return Paths.get(((JTextField) getSwingEngine().find("tfReportFILE2")).getToolTipText());
		}
	}

	JTextField getDssResultPathBaseField()
	{
		return ((JTextField) getSwingEngine().find("tfReportFILE1"));
	}

	JTextField getDssResultPathAltField()
	{
		return ((JTextField) getSwingEngine().find("tfReportFILE2"));
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

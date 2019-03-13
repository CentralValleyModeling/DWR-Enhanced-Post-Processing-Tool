/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.scenarioconfig;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import javax.swing.border.TitledBorder;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import gov.ca.water.quickresults.ui.EpptPanel;
import org.apache.log4j.Logger;
import org.jfree.data.time.Month;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-21-2019
 */
public final class ScenarioConfigurationPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(ScenarioConfigurationPanel.class.getName());
	private static final String SCENARIO_CONFIGURATION_XML_FILE = "Scenario_Configuration.xml";
	private static final ScenarioConfigurationPanel SINGLETON = new ScenarioConfigurationPanel();
	private static IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
	private final ScenarioConfigurationIO _scenarioConfigurationIO;

	private ScenarioConfigurationPanel()
	{
		try
		{
			_scenarioConfigurationIO = new ScenarioConfigurationIO();
			super.setLayout(new BorderLayout());
			Container swixmlScenarioConfigurationPanel = renderSwixml(SCENARIO_CONFIGURATION_XML_FILE);
			super.add(swixmlScenarioConfigurationPanel);
			initModels();
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + SCENARIO_CONFIGURATION_XML_FILE, e);
			throw new IllegalStateException(e);
		}
	}

	private void initModels()
	{
		// Set up month spinners on result page
		JSpinner spnSM = (JSpinner) getSwingEngine().find("spnStartMonth");
		ResultUtilsBO.SetMonthModelAndIndex(spnSM, 9, null, true);
		JSpinner spnEM = (JSpinner) getSwingEngine().find("spnEndMonth");
		ResultUtilsBO.SetMonthModelAndIndex(spnEM, 8, null, true);
		// Set up year spinners
		JSpinner spnSY = (JSpinner) getSwingEngine().find("spnStartYear");
		ResultUtilsBO.SetNumberModelAndIndex(spnSY, 1921, 1921, 2003, 1, "####", null, true);
		JSpinner spnEY = (JSpinner) getSwingEngine().find("spnEndYear");
		ResultUtilsBO.SetNumberModelAndIndex(spnEY, 2003, 1921, 2003, 1, "####", null, true);

		JCheckBox summaryTableCheckbox = (JCheckBox) getSwingEngine().find("RepckbSummaryTable");
		summaryTableCheckbox.addActionListener(e ->
		{
			boolean selected = summaryTableCheckbox.isSelected();
			Container controls3 = (Container) getSwingEngine().find("controls3");
			setSummaryTableEnabled(selected, controls3);
		});
	}

	private void setSummaryTableEnabled(boolean selected, Container container)
	{
		container.setEnabled(selected);
		for(Component component : container.getComponents())
		{
			component.setEnabled(selected);
			if(component instanceof Container)
			{
				setSummaryTableEnabled(selected, (Container) component);
			}
		}
	}

	public static ScenarioConfigurationPanel getScenarioConfigurationPanel()
	{
		return SINGLETON;
	}

	/**
	 * This method is for unit testing purposes only
	 *
	 * @return a new ScenarioConfigurationPanel with UI initialized
	 */
	static ScenarioConfigurationPanel createScenarioConfigurationPanel()
	{
		return new ScenarioConfigurationPanel();
	}


	@Override
	public String getJavaHelpId()
	{
		return "Viewing Results";
	}

	private Component getSelectedList()
	{
		return getSwingEngine().find("SelectedList");
	}

	JPanel getControls2()
	{
		return (JPanel) getSwingEngine().find("controls2");
	}

	public List<RBListItemBO> getScenarios()
	{
		List<RBListItemBO> retval = new ArrayList<>();
		Component component = getSelectedList();
		if(component instanceof JList)
		{
			JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) component;
			ListModel<RBListItemBO> model = lstScenarios.getModel();
			for(int i = 0; i < model.getSize(); i++)
			{
				retval.add(model.getElementAt(i));
			}
		}
		return retval;
	}

	public String quickState()
	{

		try
		{
			String cAdd = "";
			// Base, Comparison and Difference
			JRadioButton rdb = (JRadioButton) getSwingEngine().find("rdbp000");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Base";
			}

			rdb = (JRadioButton) getSwingEngine().find("rdbp001");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Comp";
			}

			rdb = (JRadioButton) getSwingEngine().find("rdbp002");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Diff";
			}
			// Units
			rdb = (JRadioButton) getSwingEngine().find("rdbCFS");
			if(rdb.isSelected())
			{
				cAdd = cAdd + ";CFS";
			}
			else
			{
				cAdd = cAdd + ";TAF";
			}

			// Date
			JSpinner spnSM = (JSpinner) getSwingEngine().find("spnStartMonth");
			JSpinner spnEM = (JSpinner) getSwingEngine().find("spnEndMonth");
			JSpinner spnSY = (JSpinner) getSwingEngine().find("spnStartYear");
			JSpinner spnEY = (JSpinner) getSwingEngine().find("spnEndYear");
			String cDate = spnSM.getValue().toString() + spnSY.getValue().toString();
			cDate = cDate + "-" + spnEM.getValue().toString() + spnEY.getValue().toString();
			cAdd = cAdd + ";" + cDate;

			// Time Series
			JCheckBox ckb = (JCheckBox) getSwingEngine().find("RepckbTimeSeriesPlot");
			if(ckb.isSelected())
			{
				cAdd = cAdd + ";TS";
			}

			// Exceedance Plot
			// Modify processing to reflect deprecation of "Exceedance plot"
			// checkbox - Tad 2/23/17

			StringBuilder cST = new StringBuilder();
			Component[] controls2 = ((JPanel) getSwingEngine().find("controls2")).getComponents();
			addExceedancePlots(cST, controls2);
			if(cST.length() > 0)
			{
				cAdd = cAdd + ";EX-" + cST;
			}

			// Boxplot
			if(((JCheckBox) getSwingEngine().find("RepckbBAWPlot")).isSelected())
			{
				cAdd = cAdd + ";BP";
			}
			// Monthly Table
			ckb = (JCheckBox) getSwingEngine().find("RepckbMonthlyTable");
			if(ckb.isSelected())
			{
				cAdd = cAdd + ";Monthly";
			}

			// Summary Table
			JPanel controls3 = (JPanel) getSwingEngine().find("controls3");
			Component[] components = controls3.getComponents();
			ckb = (JCheckBox) getSwingEngine().find("RepckbSummaryTable");
			if(ckb.isSelected())
			{
				cST = new StringBuilder(";ST-");
				addSummaryTables(cST, components);
				cAdd = cAdd + cST;
			}

			return cAdd;
		}
		catch(RuntimeException e)
		{
			LOGGER.error(e.getMessage(), e);
			String messageText = "Unable to display frame.";
			errorHandlingSvc.businessErrorHandler(messageText, e);
		}
		return null;
	}

	private void addExceedancePlots(StringBuilder cST, Component[] components)
	{
		for(Component c : components)
		{
			if(c instanceof JCheckBox && ((JCheckBox) c).isSelected())
			{
				cST.append(",").append(((JCheckBox) c).getText());
			}
			else if(c instanceof Container)
			{
				addExceedancePlots(cST, ((Container) c).getComponents());
			}
		}
	}

	private void addSummaryTables(StringBuilder cST, Component[] components)
	{
		for(final Component component : components)
		{
			if(component instanceof JCheckBox)
			{
				JCheckBox c = (JCheckBox) component;
				if(c.isSelected())
				{
					String cName = c.getText();
					cST.append(",").append(cName);
				}
			}
			else if(component instanceof Container)
			{
				addSummaryTables(cST, ((Container) component).getComponents());
			}
		}
	}

	public Month getStartMonth()
	{
		JSpinner monthSpinner = (JSpinner) getSwingEngine().find("spnStartMonth");
		int month = ResultUtilsBO.getResultUtilsInstance(null).monthToInt(monthSpinner.getValue().toString());
		JSpinner yearSpinner = (JSpinner) getSwingEngine().find("spnStartYear");
		int year = Integer.parseInt(yearSpinner.getValue().toString());
		return new Month(month, year);
	}

	public Month getEndMonth()
	{
		JSpinner monthSpinner = (JSpinner) getSwingEngine().find("spnEndMonth");
		int month = ResultUtilsBO.getResultUtilsInstance(null).monthToInt(monthSpinner.getValue().toString());
		JSpinner yearSpinner = (JSpinner) getSwingEngine().find("spnEndYear");
		int year = Integer.parseInt(yearSpinner.getValue().toString());
		return new Month(month, year);
	}

	public void saveConfigurationToPath(Path selectedPath) throws IOException
	{
		_scenarioConfigurationIO.saveConfiguration(selectedPath);
		EpptPreferences.setLastScenarioConfiguration(selectedPath);
	}

	public void loadScenarioConfiguration(Path selectedPath) throws IOException
	{
		if(selectedPath.toFile().exists())
		{
			_scenarioConfigurationIO.loadConfiguration(selectedPath);
			JPanel panel = (JPanel) getSwingEngine().find("ss");
			panel.setBorder(new TitledBorder("Scenarios - " + selectedPath.getFileName()));
		}
	}

	JList<?> getScenarioList()
	{
		return (JList<?>) getSwingEngine().find("SelectedList");
	}

	JRadioButton getRadioButton1()
	{
		return (JRadioButton) getSwingEngine().find("rdbp001");
	}

	JRadioButton getRadioButton2()
	{
		return (JRadioButton) getSwingEngine().find("rdbp002");
	}

	JLabel getLabelBase()
	{
		return (JLabel) getSwingEngine().find("lblBase");
	}

	void setStartMonth(Month start)
	{
		JSpinner monthSpinner = (JSpinner) getSwingEngine().find("spnStartMonth");
		monthSpinner.setValue(ResultUtilsBO.getResultUtilsInstance(null).intToMonth(start.getMonth()));
		JSpinner yearSpinner = (JSpinner) getSwingEngine().find("spnStartYear");
		yearSpinner.setValue(start.getYearValue());
	}

	void setEndMonth(Month end)
	{
		JSpinner monthSpinner = (JSpinner) getSwingEngine().find("spnEndMonth");
		monthSpinner.setValue(ResultUtilsBO.getResultUtilsInstance(null).intToMonth(end.getMonth()));
		JSpinner yearSpinner = (JSpinner) getSwingEngine().find("spnEndYear");
		yearSpinner.setValue(end.getYearValue());
	}

	public void setScenarios(List<RBListItemBO> scenarios)
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
		JRadioButton radioButton = (JRadioButton) scenarioConfigurationPanel.getSwingEngine().find("rdbp001");
		radioButton.setSelected(true);
		Component component = scenarioConfigurationPanel.getSwingEngine().find("SelectedList");
		if(component instanceof JList)
		{
			JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) component;
			DefaultListModel<RBListItemBO> defaultModel = new DefaultListModel<>();
			for(RBListItemBO item : scenarios)
			{
				defaultModel.addElement(item);
			}
			lstScenarios.setModel(defaultModel);
		}
	}
}

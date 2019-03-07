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
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
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
public class ScenarioConfigurationPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(ScenarioConfigurationPanel.class.getName());
	private static final String SCENARIO_CONFIGURATION_XML_PATH = "ui/Scenario_Configuration.xml";
	private static final ScenarioConfigurationPanel SINGLETON = new ScenarioConfigurationPanel();
	private static IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();

	private ScenarioConfigurationPanel()
	{
		try
		{
			super.setLayout(new BorderLayout());
			Container swixmlScenarioConfigurationPanel = getSwingEngine().render(SCENARIO_CONFIGURATION_XML_PATH);
			super.add(swixmlScenarioConfigurationPanel);
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
		}
		catch(Exception e)
		{
			throw new IllegalStateException(e);
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
			for(Component c : ((JPanel) getSwingEngine().find("controls2")).getComponents())
			{
				if(c instanceof JCheckBox && ((JCheckBox) c).isSelected())
				{
					cST.append(",").append(((JCheckBox) c).getText());
				}
			}
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
				}
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

}

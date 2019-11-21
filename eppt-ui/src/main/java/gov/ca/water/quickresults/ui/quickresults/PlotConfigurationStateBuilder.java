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

package gov.ca.water.quickresults.ui.quickresults;

import java.awt.Component;
import java.awt.Container;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import org.swixml.SwingEngine;

public class PlotConfigurationStateBuilder
{
	private static final Logger LOGGER = Logger.getLogger(PlotConfigurationStateBuilder.class.getName());
	private static IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
	private final SwingEngine _swingEngine;

	public PlotConfigurationStateBuilder(SwingEngine swingEngine)
	{
		_swingEngine = swingEngine;
	}

	public String createQuickStateString()
	{
		try
		{
			String cAdd = "";
			// Base, Comparison and Difference
			JRadioButton rdb = (JRadioButton) _swingEngine.find("rdbp000");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Base";
			}

			rdb = (JRadioButton) _swingEngine.find("rdbp001");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Comp";
			}

			rdb = (JRadioButton) _swingEngine.find("rdbp002");
			if(rdb.isSelected())
			{
				cAdd = cAdd + "Diff";
			}
			// Units
			rdb = (JRadioButton) _swingEngine.find("rdbCFS");
			if(rdb.isSelected())
			{
				cAdd = cAdd + ";CFS";
			}
			else
			{
				cAdd = cAdd + ";TAF";
			}

			// Date
			JSpinner spnSM = (JSpinner) _swingEngine.find("spnStartMonth");
			JSpinner spnEM = (JSpinner) _swingEngine.find("spnEndMonth");
			JSpinner spnSY = (JSpinner) _swingEngine.find("spnStartYear");
			JSpinner spnEY = (JSpinner) _swingEngine.find("spnEndYear");

			String cDate = spnSM.getValue().toString() + spnSY.getValue().toString();
			cDate = cDate + "-" + spnEM.getValue().toString() + spnEY.getValue().toString();
			cAdd = cAdd + ";" + cDate;

			// Time Series
			JCheckBox ckb = (JCheckBox) _swingEngine.find("RepckbTimeSeriesPlot");
			if(ckb.isSelected())
			{
				cAdd = cAdd + ";TS";
			}

			StringBuilder cST = new StringBuilder();
			Component[] controls2 = ((JPanel) _swingEngine.find("controls2")).getComponents();
			addExceedancePlots(cST, controls2);
			if(cST.length() > 0)
			{
				cAdd = cAdd + ";EX-" + cST;
			}

			// Boxplot
			if(((JCheckBox) _swingEngine.find("RepckbBAWPlot")).isSelected())
			{
				cAdd = cAdd + ";BP";
			}
			// Monthly Table
			ckb = (JCheckBox) _swingEngine.find("RepckbMonthlyTable");
			if(ckb.isSelected())
			{
				cAdd = cAdd + ";Monthly";
			}

			// Summary Table
			JPanel controls3 = (JPanel) _swingEngine.find("controls3");
			Component[] components = controls3.getComponents();
			ckb = (JCheckBox) _swingEngine.find("RepckbSummaryTable");
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
			LOGGER.log(Level.SEVERE, "Error building plot configuration state", e);
			String messageText = "Unable to display frame.";
			errorHandlingSvc.businessErrorHandler(messageText, e);
			return "";
		}
	}

	public PlotConfigurationState createPlotConfigurationState()
	{
		try
		{
			// Base, Comparison and Difference
			PlotConfigurationState.ComparisonType comparisonType = PlotConfigurationState.ComparisonType.BASE;
			if(((JRadioButton) _swingEngine.find("rdbp001")).isSelected())
			{
				comparisonType = PlotConfigurationState.ComparisonType.COMPARISON;
			}
			if(((JRadioButton) _swingEngine.find("rdbp002")).isSelected())
			{
				comparisonType = PlotConfigurationState.ComparisonType.DIFF;
			}
			boolean displayTaf = !((JCheckBox) _swingEngine.find("chkTAF")).isSelected();
			boolean displayTimeSeriesPlot = ((JCheckBox) _swingEngine.find("RepckbTimeSeriesPlot")).isSelected();
			boolean displayBoxAndWhisker = ((JCheckBox) _swingEngine.find("RepckbBAWPlot")).isSelected();
			boolean displayMonthlyTable = ((JCheckBox) _swingEngine.find("RepckbMonthlyTable")).isSelected();
			boolean displaySummaryTable = ((JCheckBox) _swingEngine.find("RepckbSummaryTable")).isSelected();

			Component[] controls2 = ((JPanel) _swingEngine.find("controls2")).getComponents();
			List<String> exceedance = new ArrayList<>();
			addExceedancePlots(exceedance, controls2);

			List<String> summary = new ArrayList<>();
			// Summary Table
			if(displaySummaryTable)
			{
				Component[] components = ((JPanel) _swingEngine.find("controls3")).getComponents();
				addSummaryTables(summary, components);
			}
			return new PlotConfigurationState(comparisonType, displayTaf, displayTimeSeriesPlot, displayBoxAndWhisker, exceedance,
					displayMonthlyTable, displaySummaryTable, summary);
		}
		catch(RuntimeException e)
		{
			LOGGER.log(Level.SEVERE, "Error building plot configuration state", e);
			String messageText = "Unable to display frame.";
			errorHandlingSvc.businessErrorHandler(messageText, e);
			return null;
		}
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

	private void addExceedancePlots(List<String> exceedance, Component[] components)
	{
		for(Component c : components)
		{
			if(c instanceof JCheckBox && ((JCheckBox) c).isSelected())
			{
				exceedance.add(((JCheckBox) c).getText());
			}
			else if(c instanceof Container)
			{
				addExceedancePlots(exceedance, ((Container) c).getComponents());
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

	private void addSummaryTables(List<String> summary, Component[] components)
	{
		for(final Component component : components)
		{
			if(component instanceof JCheckBox)
			{
				JCheckBox c = (JCheckBox) component;
				if(c.isSelected())
				{
					String cName = c.getText();
					summary.add(cName);
				}
			}
			else if(component instanceof Container)
			{
				addSummaryTables(summary, ((Container) component).getComponents());
			}
		}
	}
}

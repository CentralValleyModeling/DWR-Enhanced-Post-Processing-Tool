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
import java.time.Month;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.TextStyle;
import java.time.temporal.TemporalAccessor;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.swing.*;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.ScriptedEpptStatistics;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import org.swixml.SwingEngine;

import static java.util.stream.Collectors.joining;

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
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			List<EpptScenarioRun> epptScenarioAlternatives = projectConfigurationPanel.getEpptScenarioAlternatives();
			if(epptScenarioAlternatives.isEmpty())
			{
				cAdd = cAdd + "Base";
			}
			else
			{

				JCheckBox checkBox = (JCheckBox) _swingEngine.find("RepckbScenarioDiff");
				if(checkBox.isSelected())
				{
					cAdd = cAdd + "Diff";
				}
				else
				{
					cAdd = cAdd + "Comp";
				}
			}
			// Units
			JCheckBox checkBox = (JCheckBox) _swingEngine.find("chkTAF");
			if(!checkBox.isSelected())
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

			Component[] controls2 = ((JPanel) _swingEngine.find("controls2")).getComponents();
			List<Month> exceedanceMonths = new ArrayList<>();
			boolean isDisplayExceedanceAll = ((JCheckBox) _swingEngine.find("ExceedanceALL")).isSelected();
			boolean isDisplayExceedanceAnnualFlow = ((JCheckBox) _swingEngine.find("ExceedanceAnnual")).isSelected();
			addExceedancePlots(exceedanceMonths, controls2);
			if(!exceedanceMonths.isEmpty() || isDisplayExceedanceAll || isDisplayExceedanceAnnualFlow)
			{
				cAdd = cAdd + ";EX-" + exceedanceMonths.stream().map(m -> m.getDisplayName(TextStyle.SHORT, Locale.getDefault()))
													   .collect(joining(","));
				if(isDisplayExceedanceAll)
				{
					cAdd += ",All";
				}
				if(isDisplayExceedanceAnnualFlow)
				{
					cAdd += ",Annual";
				}
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
				List<EpptStatistic> summary = new ArrayList<>();
				addSummaryTables(summary, components);
				cAdd = cAdd + ";ST-" + summary.stream().map(EpptStatistic::getName).collect(Collectors.joining(","));
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
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			WaterYearDefinition waterYearDefinition = projectConfigurationPanel.getWaterYearDefinition();
			List<EpptScenarioRun> epptScenarioAlternatives = projectConfigurationPanel.getEpptScenarioAlternatives();
			PlotConfigurationState.ComparisonType comparisonType;
			if(epptScenarioAlternatives.isEmpty())
			{
				comparisonType = PlotConfigurationState.ComparisonType.BASE;
			}
			else
			{
				JCheckBox checkBox = (JCheckBox) _swingEngine.find("RepckbScenarioDiff");
				if(checkBox.isSelected())
				{
					comparisonType = PlotConfigurationState.ComparisonType.DIFF;
				}
				else
				{
					comparisonType = PlotConfigurationState.ComparisonType.COMPARISON;
				}
			}
			boolean displayTaf = !((JCheckBox) _swingEngine.find("chkTAF")).isSelected();
			boolean displayTimeSeriesPlot = ((JCheckBox) _swingEngine.find("RepckbTimeSeriesPlot")).isSelected();
			boolean displayBoxAndWhisker = ((JCheckBox) _swingEngine.find("RepckbBAWPlot")).isSelected();
			boolean displayMonthlyTable = ((JCheckBox) _swingEngine.find("RepckbMonthlyTable")).isSelected();
			boolean displaySummaryTable = ((JCheckBox) _swingEngine.find("RepckbSummaryTable")).isSelected();
			boolean isDisplayExceedanceAll = ((JCheckBox) _swingEngine.find("ExceedanceALL")).isSelected();
			boolean isDisplayExceedanceAnnualFlow = ((JCheckBox) _swingEngine.find("ExceedanceAnnual")).isSelected();

			Component[] controls2 = ((JPanel) _swingEngine.find("controls2")).getComponents();
			List<Month> exceedance = new ArrayList<>();
			addExceedancePlots(exceedance, controls2);

			List<EpptStatistic> summary = new ArrayList<>();
			// Summary Table
			if(displaySummaryTable)
			{
				Component[] components = ((JPanel) _swingEngine.find("controls3")).getComponents();
				addSummaryTables(summary, components);
			}
			return new PlotConfigurationState(comparisonType, displayTaf, displayTimeSeriesPlot, displayBoxAndWhisker, isDisplayExceedanceAll,
					exceedance, displayMonthlyTable, displaySummaryTable, isDisplayExceedanceAnnualFlow, summary,
					new ArrayList<>(), waterYearDefinition);
		}
		catch(RuntimeException e)
		{
			LOGGER.log(Level.SEVERE, "Error building plot configuration state", e);
			String messageText = "Unable to display frame.";
			errorHandlingSvc.businessErrorHandler(messageText, e);
			return null;
		}
	}


	private void addExceedancePlots(List<Month> exceedanceMonths, Component[] components)
	{
		for(Component c : components)
		{
			if(c instanceof JCheckBox && ((JCheckBox) c).isSelected())
			{
				if(c.getName().toLowerCase().startsWith("repchkmon"))
				{
					String text = ((JCheckBox) c).getText();
					TemporalAccessor mmm = new DateTimeFormatterBuilder()
							.parseCaseInsensitive()
							.appendPattern("MMM")
							.toFormatter()
							.parse(text);
					exceedanceMonths.add(Month.from(mmm));
				}
			}
			else if(c instanceof Container)
			{
				addExceedancePlots(exceedanceMonths, ((Container) c).getComponents());
			}
		}
	}

	private void addSummaryTables(List<EpptStatistic> summaryTables, Component[] components)
	{
		List<EpptStatistic> trendStatistics = ScriptedEpptStatistics.getTrendStatistics();
		for(final Component component : components)
		{
			if(component instanceof JCheckBox)
			{
				JCheckBox c = (JCheckBox) component;
				if(c.isSelected())
				{
					String cName = c.getText();
					trendStatistics.stream().filter(e -> e.getName().equalsIgnoreCase(cName)).findAny()
								   .ifPresent(summaryTables::add);
				}
			}
			else if(component instanceof Container)
			{
				addSummaryTables(summaryTables, ((Container) component).getComponents());
			}
		}
	}
}

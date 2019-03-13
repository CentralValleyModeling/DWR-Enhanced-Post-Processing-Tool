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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.presentation.WRIMSGUILinks;
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
	private DefaultListModel<RBListItemBO> _lmScenNames;

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
		Component component = getSwingEngine().find("SelectedList");
		if(component instanceof JList)
		{
			JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) component;
			lstScenarios.getSelectionModel().addListSelectionListener(e -> setModified(true));
			lstScenarios.getModel().addListDataListener(new ListDataListener()
			{
				@Override
				public void intervalAdded(ListDataEvent e)
				{
					getRadioButton1().setEnabled(getScenarios().size() > 1);
					getRadioButton2().setEnabled(getScenarios().size() > 1);
					setModified(true);
				}

				@Override
				public void intervalRemoved(ListDataEvent e)
				{
					getRadioButton1().setEnabled(getScenarios().size() > 1);
					getRadioButton2().setEnabled(getScenarios().size() > 1);
					setModified(true);
				}

				@Override
				public void contentsChanged(ListDataEvent e)
				{
					getRadioButton1().setEnabled(getScenarios().size() > 1);
					getRadioButton2().setEnabled(getScenarios().size() > 1);
					setModified(true);
				}
			});
		}
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

		_lmScenNames = new DefaultListModel<>();
		_lmScenNames.addListDataListener(new MyListDataListener());
		getScenarioList().setModel(_lmScenNames);
		getScenarioList().setCellRenderer(new RBListRenderer());
		getScenarioList().setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		getScenarioList().addMouseListener(new MouseAdapter()
		{
			@Override
			public void mouseClicked(MouseEvent event)
			{

				JList list = (JList) event.getSource();

				// Get index of item clicked

				if(list.getModel().getSize() > 0)
				{
					int index = list.locationToIndex(event.getPoint());

					// Toggle selected state

					for(int i = 0; i < list.getModel().getSize(); i++)
					{
						RBListItemBO item = (RBListItemBO) list.getModel().getElementAt(i);
						if(i == index)
						{
							item.setSelected(true);
							list.repaint(list.getCellBounds(i, i));
						}
						else
						{
							if(item.isSelected())
							{
								list.repaint(list.getCellBounds(i, i));
							}
							item.setSelected(false);
						}
					}

					// Repaint cell

					list.repaint(list.getCellBounds(index, index));

					WRIMSGUILinks.updateProjectFiles(list);

				}
			}
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

	JList<RBListItemBO> getScenarioList()
	{
		return (JList<RBListItemBO>) getSwingEngine().find("SelectedList");
	}

	DefaultListModel<RBListItemBO> getLmScenNames()
	{
		return _lmScenNames;
	}

	JRadioButton getRadioButton1()
	{
		return (JRadioButton) getSwingEngine().find("rdbp001");
	}

	JRadioButton getRadioButton2()
	{
		return (JRadioButton) getSwingEngine().find("rdbp002");
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
		JRadioButton radioButton = (JRadioButton) getSwingEngine().find("rdbp001");
		radioButton.setSelected(true);
		Component component = getSwingEngine().find("SelectedList");
		if(component instanceof JList)
		{
			JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) component;
			_lmScenNames.removeAllElements();
			for(RBListItemBO item : scenarios)
			{
				_lmScenNames.addElement(item);
			}
			lstScenarios.setModel(_lmScenNames);
			scenarioListChanged();
		}
	}

	private void scenarioListChanged()
	{
		getRadioButton1().setEnabled(_lmScenNames.getSize() > 1);
		getRadioButton2().setEnabled(_lmScenNames.getSize() > 1);
		if(_lmScenNames.getSize() <= 1)
		{
			JRadioButton radioButton = (JRadioButton) getSwingEngine().find("rdbp000");
			radioButton.setSelected(true);
		}
		WRIMSGUILinks.updateProjectFiles(getScenarioList());
		ScenarioConfigurationPanel.this.setModified(true);
		getScenarioList().setModel(_lmScenNames);
		getScenarioList().invalidate();
		getScenarioList().revalidate();
		getScenarioList().repaint();
	}

	/**
	 * Custom class to show radiobutton items in place of textfields in a list
	 *
	 * @author tslawecki
	 */
	private class RBListRenderer extends JRadioButton implements ListCellRenderer
	{
		@Override
		public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
													  boolean hasFocus)
		{
			setEnabled(list.isEnabled());
			setSelected(((RBListItemBO) value).isSelected());
			setFont(list.getFont());
			setBackground(list.getBackground());
			setForeground(list.getForeground());
			setText(((RBListItemBO) value).getLabel());
			this.setToolTipText(value.toString() + " 	\n" + ((RBListItemBO) value).getSVFilename());
			return this;
		}
	}

	/**
	 * Custom ListDataListener to enable/disable controls based on number of
	 * files in list.
	 *
	 * @author tslawecki
	 */
	private class MyListDataListener implements ListDataListener
	{
		@Override
		public void contentsChanged(ListDataEvent e)
		{
			scenarioListChanged();
		}

		@Override
		public void intervalAdded(ListDataEvent e)
		{
			scenarioListChanged();
		}

		@Override
		public void intervalRemoved(ListDataEvent e)
		{
			scenarioListChanged();
		}

	}
}

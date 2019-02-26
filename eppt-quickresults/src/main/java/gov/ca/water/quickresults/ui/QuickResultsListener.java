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
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bo.ResultUtilsBO;
import gov.ca.water.calgui.presentation.DisplayFrame;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import org.apache.log4j.Logger;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
class QuickResultsListener implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsListener.class.getName());
	private final ScenarioConfigurationPanel _scenarioConfigurationPanel;
	private final QuickResultsPanel _quickResultsPanel;

	QuickResultsListener(QuickResultsPanel quickResultsPanel, ScenarioConfigurationPanel scenarioConfigurationPanel)
	{
		_quickResultsPanel = quickResultsPanel;
		_scenarioConfigurationPanel = scenarioConfigurationPanel;
	}

	@Override
	public void actionPerformed(ActionEvent e)
	{
		switch(e.getActionCommand())
		{
			case "Rep_AddList":
				addToReportList();
				break;
			case "Rep_ClearList":
				clearReportList();
				break;
			case "Rep_DispAll":
				displayReportList();
				break;
			case "Rep_SaveList":
				saveReportList();
				break;
			case "Rep_LoadList":
				loadReportList();
				break;
			default:
				LOGGER.info("Testing action events " + e.getActionCommand());
		}
	}

	private void loadReportList()
	{
		Optional<List<String>> data = ResultUtilsBO.getResultUtilsInstance(null).readCGR();
		if(data.isPresent())
		{
			Component component = _quickResultsPanel.getReportsJList();
			if(component instanceof JList)
			{
				JList<String> lstReports = (JList<String>) component;
				lstReports.setListData(data.get().toArray(new String[0]));
			}
		}
	}

	private void saveReportList()
	{
		ResultUtilsBO.getResultUtilsInstance(null).writeCGR();
	}

	private void displayReportList()
	{
		List<RBListItemBO> scenarios = _scenarioConfigurationPanel.getScenarios();
		if(scenarios.isEmpty())
		{
			IDialogSvc dialogSvc = DialogSvcImpl.getDialogSvcInstance();
			dialogSvc.getOK("Error - No scenarios loaded", JOptionPane.ERROR_MESSAGE);
		}
		else
		{
			Component component = _quickResultsPanel.getReportsJList();
			if(component instanceof JList)
			{
				JList<String> lstReports = (JList<String>) component;
				for(int i = 0; i < lstReports.getModel().getSize(); i++)
				{
					DisplayFrame.showDisplayFrames(_quickResultsPanel.getSwingEngine(),
							(lstReports.getModel().getElementAt(i)),
							scenarios);
				}
			}
		}
	}

	private List<String> getReports()
	{
		List<String> retval = new ArrayList<>();
		Component component = _quickResultsPanel.getReportsJList();
		if(component instanceof JList)
		{
			JList<String> lstReports = (JList<String>) component;
			ListModel<String> model = lstReports.getModel();
			for(int i = 0; i < model.getSize(); i++)
			{
				retval.add(model.getElementAt(i));
			}
		}
		return retval;
	}

	private void clearReportList()
	{
		Component component = _quickResultsPanel.getReportsJList();
		if(component instanceof JList)
		{
			JList<String> lstReports = (JList<String>) component;
			lstReports.setListData(new String[0]);
		}
	}

	/**
	 * Adds items to list of reports on Quick Result dashboard. One item is
	 * added for each checked item on the children panels for "variables"
	 */
	private void addToReportList()
	{
		try
		{
			// Store previous list items
			List<String> reports = getReports();
			int size = reports.size();
			int n = 0;
			String[] lstArray = new String[size];
			for(Object item : reports)
			{
				if(!" ".equals(item.toString()))
				{
					lstArray[n] = item.toString();
					n = n + 1;
				}
			}
			String[] lstArray1 = new String[n + 1];
			System.arraycopy(lstArray, 0, lstArray1, 0, n);
			// Add new items
			StringBuilder cSTOR = new StringBuilder(";Locs-");
			StringBuilder cSTORIdx = new StringBuilder(";Index-");
			Component[] components = _quickResultsPanel.getVariables().getComponents();
			for(Component c : components)
			{
				if(c instanceof JPanel)
				{
					Component[] components2 = ((JPanel) c).getComponents();
					for(Component c2 : components2)
					{
						if(c2 instanceof JCheckBox)
						{
							JCheckBox cb = (JCheckBox) c2;
							String cName = cb.getName();
							if(cName.startsWith("ckbp") && cb.isSelected())
							{
								cSTOR.append(cb.getText().trim()).append(",");
								cSTORIdx.append(cName).append(",");
							}
							lstArray1[n] = DisplayFrame.quickState(
									_scenarioConfigurationPanel.getSwingEngine()) + cSTOR + cSTORIdx;

							Component component = _quickResultsPanel.getReportsJList();
							if(component instanceof JList)
							{
								JList<String> lstReports = (JList<String>) component;
								lstReports.setListData(lstArray1);
							}
						}
					}
				}
			}
		}
		catch(RuntimeException e)
		{
			LOGGER.error(e.getMessage(), e);
			String messageText = "Unable to display frame.";
			IErrorHandlingSvc errorHandlingSvc = new ErrorHandlingSvcImpl();
			Window windowAncestor = SwingUtilities.getWindowAncestor(_quickResultsPanel.getReportsJList());
			errorHandlingSvc.businessErrorHandler(messageText, (JFrame) windowAncestor, e);
		}
	}
}

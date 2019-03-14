/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.customresults;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionListener;
import java.util.List;
import javax.swing.*;

import calsim.app.AppUtils;
import calsim.app.DerivedTimeSeries;
import calsim.app.MultipleTimeSeries;
import calsim.gui.GuiUtils;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.presentation.WRIMSGUILinks;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.quickresults.QuickResultsPanel;
import gov.ca.water.quickresults.ui.scenarioconfig.ProjectConfigurationPanel;
import org.apache.log4j.Logger;
import org.jfree.data.time.Month;
import vista.set.DataReference;
import vista.set.Group;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-25-2019
 */
public class CustomResultsPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanel.class.getName());
	private static final String CUSTOM_RESULTS_XML_FILE = "Custom_Results.xml";
	private final IErrorHandlingSvc _errorHandlingSvc = new ErrorHandlingSvcImpl();
	private final IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();
	private final DisplayHelper _displayHelper;

	public CustomResultsPanel()
	{
		try
		{
			_displayHelper = new DisplayHelper(this);
			super.setLayout(new BorderLayout());
			Container swixmlCustomResultsPanel = renderSwixml(CUSTOM_RESULTS_XML_FILE);
			super.add(swixmlCustomResultsPanel);
			WRIMSGUILinks.buildWRIMSGUI((JPanel) getSwingEngine().find("WRIMS"));
			JButton retrieveBtn = GuiUtils.getCLGPanel().getRetrievePanel().getRetrieveBtn();
			for(ActionListener al : retrieveBtn.getActionListeners())
			{
				retrieveBtn.removeActionListener(al);
			}
			retrieveBtn.addActionListener(arg0 -> retrieve());
			Component openButtonComponent = findFirstButtonMatchingText(GuiUtils.getCLGPanel(), "Open");
			if(openButtonComponent != null)
			{
				JButton openButton = (JButton) openButtonComponent;
				for(ActionListener al : openButton.getActionListeners())
				{
					openButton.removeActionListener(al);
				}
				openButton.addActionListener(arg0 -> retrieve2());
			}
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + CUSTOM_RESULTS_XML_FILE, e);
			throw new IllegalStateException(e);
		}
	}

	@Override
	public String getJavaHelpId()
	{
		return "Custom Results";
	}

	/**
	 * Helper function that scans GUI for a button with the indicated label
	 * starting with a given component. Used in CalLite GUI to find the "Open"
	 * button on the CLG panel and replace the associated action.
	 *
	 * @param comp Starting component. Function recurses through childre.n
	 * @param text Text to match on JButton
	 * @return JButton component with specified label
	 */
	private static Component findFirstButtonMatchingText(Component comp, String text)
	{

		if((comp instanceof JButton) && ((JButton) comp).getText().equals(text))
		{
			return comp;
		}

		if(comp instanceof Container)
		{
			Container container = (Container) comp;
			for(int i = 0; i < container.getComponentCount(); i++)
			{
				Component comp2 = findFirstButtonMatchingText(container.getComponent(i), text);
				if(comp2 != null)
				{
					return comp2;
				}
			}
		}
		return null;
	}

	/**
	 * Data retrieval for single DSS from Custom Results dashboard; modeled on
	 * calsim.gui.GeneralRetrievePanel.retrieve()
	 */
	private void retrieve()
	{
		if(!AppUtils.baseOn)
		{
			_dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);
			return;
		}
		try
		{
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			List<RBListItemBO> scenarios = projectConfigurationPanel.getScenarios();
			String noRowsString = "";
			JTable table = GuiUtils.getCLGPanel().getRetrievePanel().getTable();
			if(table.getRowCount() == 0)
			{
				noRowsString = " after using \"Filter\" to load variables";
			}
			Group group = GuiUtils.getCLGPanel().getRetrievePanel().getGroup();
			if(group == null || table.getSelectedRowCount() == 0)
			{
				_dialogSvc.getOK("Variables not selected! Select one or more variables" + noRowsString,
						JOptionPane.INFORMATION_MESSAGE);
				return;
			}
			// checked if count > 0 above
			int[] rows = table.getSelectedRows();
			DataReference[] array = new DataReference[rows.length];
			for(int i = 0; i < rows.length; i++)
			{
				array[i] = group.getDataReference(rows[i]);
			}
			for(int i = 0; i < rows.length; i++)
			{
				String[] parts = array[i].getName().split("::");
				String[] parts2 = parts[2].split("/");
				parts[2] = "/" + parts2[1] + "/" + parts2[2] + "/" + parts2[3] + "/" + parts[3] + "/" + parts2[5] + "/"
						+ parts2[6] + "/";
				String quickState = projectConfigurationPanel.quickState();
				Month startMonth = projectConfigurationPanel.getStartMonth();
				Month endMonth = projectConfigurationPanel.getEndMonth();
				if(parts[1].toUpperCase().contains(("_SV.DSS")))
				{
					_displayHelper.showDisplayFrames(quickState + ";Locs-" + parts[2] + ";Index-"
							+ parts[2] + ";File-" + parts[1], scenarios, startMonth, endMonth);
				}
				else
				{
					_displayHelper.showDisplayFrames(
							quickState + ";Locs-" + parts[2] + ";Index-" + parts[2],
							scenarios, startMonth, endMonth);
				}
			}
		}
		catch(RuntimeException e)
		{
			LOGGER.debug("Error in retrieve() -", e);
		}
	}

	/**
	 * Data retrieval for DTS/MTS from Custom Results dashboard; modeled on
	 * calsim.gui.GeneralRetrievePanel.retrieve()
	 */

	private void retrieve2()
	{

		if(!AppUtils.baseOn)
		{
			_dialogSvc.getOK("DSS not selected! The Base DSS files need to be selected", JOptionPane.WARNING_MESSAGE);
			return;
		}

		DerivedTimeSeries dts = GuiUtils.getCLGPanel().getDtsTreePanel().getTable().getDTS();
		MultipleTimeSeries mts = GuiUtils.getCLGPanel().getDtsTreePanel().getTable().getMTS();

		if(((mts == null) && (dts == null)) || ((dts != null) && (dts.getBParts().isEmpty()))
				|| ((mts != null) && (mts.getNumberOfDataReferences() < 1)))
		{
			_dialogSvc.getOK("Nothing to display! Specify DTS or MTS data reference", JOptionPane.WARNING_MESSAGE);

			return;
		}

		try
		{
			ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
			String quickState = projectConfigurationPanel.quickState();
			Month startMonth = projectConfigurationPanel.getStartMonth();
			Month endMonth = projectConfigurationPanel.getEndMonth();
			List<RBListItemBO> scenarios = projectConfigurationPanel.getScenarios();
			_displayHelper.showDisplayFramesWRIMS(quickState + ";Locs-;Index-;File-", scenarios, dts,
					mts, startMonth, endMonth);

		}
		catch(Exception e)
		{
			LOGGER.debug("Error in retrieve2() -", e);
			_errorHandlingSvc.businessErrorHandler(null, e);
		}
	}
}

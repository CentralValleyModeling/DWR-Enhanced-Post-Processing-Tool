/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.quickresults;

import java.awt.HeadlessException;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.presentation.DisplayFrame;
import gov.ca.water.calgui.tech_service.IDialogSvc;
import gov.ca.water.calgui.tech_service.IErrorHandlingSvc;
import gov.ca.water.calgui.tech_service.impl.DialogSvcImpl;
import gov.ca.water.calgui.tech_service.impl.ErrorHandlingSvcImpl;
import gov.ca.water.quickresults.ui.scenarioconfig.ProjectConfigurationPanel;
import org.apache.log4j.Logger;
import org.jfree.data.time.Month;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-06-2019
 */
class QuickResultsMouseListener extends MouseAdapter
{
	private static final Logger LOG = Logger.getLogger(QuickResultsMouseListener.class.getName());
	private final IErrorHandlingSvc _errorHandlingSvc = new ErrorHandlingSvcImpl();
	private final IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();

	@Override
	public void mouseClicked(MouseEvent e)
	{
		try
		{
			LOG.debug("mouseClicked");
			JComponent component = (JComponent) e.getComponent();
			String cName = component.getName();

			if(!SwingUtilities.isRightMouseButton(e))
			{
				// checkbox from quick results.
				int iClickCount = e.getClickCount();
				if(iClickCount == 2 && cName.startsWith("ckbp"))
				{
					// Double Click
					JCheckBox chk = (JCheckBox) component;
					List<RBListItemBO> scenarios = ProjectConfigurationPanel.getProjectConfigurationPanel().getScenarios();
					if(scenarios.isEmpty())
					{
						_dialogSvc.getOK("Error - No scenarios loaded", JOptionPane.ERROR_MESSAGE);
					}
					else
					{
						ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
						String quickState = projectConfigurationPanel.quickState();
						Month startMonth = projectConfigurationPanel.getStartMonth();
						Month endMonth = projectConfigurationPanel.getEndMonth();
						DisplayFrame.showDisplayFrames(quickState + ";Locs-" + chk.getText()
								+ ";Index-" + chk.getName(), scenarios, startMonth, endMonth);
					}
				}
			}
		}
		catch(HeadlessException ex)
		{
			LOG.error(ex.getMessage(), ex);
			String messageText = "Unable to initialize mouse listeners.";
			_errorHandlingSvc.businessErrorHandler(messageText, ex);
		}
	}
}

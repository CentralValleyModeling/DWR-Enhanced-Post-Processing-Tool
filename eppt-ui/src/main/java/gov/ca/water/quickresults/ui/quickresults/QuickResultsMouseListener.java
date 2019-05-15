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

import java.awt.HeadlessException;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.techservice.IDialogSvc;
import gov.ca.water.calgui.techservice.IErrorHandlingSvc;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.calgui.techservice.impl.ErrorHandlingSvcImpl;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
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
	private final DisplayHelper _displayHelper;

	public QuickResultsMouseListener(DisplayHelper displayHelper)
	{
		_displayHelper = displayHelper;
	}

	@Override
	public void mouseClicked(MouseEvent e)
	{
		try
		{
			LOG.debug("mouseClicked");
			JComponent component = (JComponent) e.getComponent();
			String cName = component.getName();

			if(!SwingUtilities.isRightMouseButton(e) && e.isControlDown() && cName.startsWith("ckbp"))
			{
				// checkbox from quick results.
				JCheckBox chk = (JCheckBox) component;
				//This is to undo the click event action
				chk.setSelected(!chk.isSelected());
				List<EpptScenarioRun> alternatives = ProjectConfigurationPanel.getProjectConfigurationPanel().getEpptScenarioAlternatives();
				EpptScenarioRun baseScenario = ProjectConfigurationPanel.getProjectConfigurationPanel().getBaseScenario();
				if(baseScenario == null)
				{
					_dialogSvc.getOK("Error - No Base Scenario defined", JOptionPane.ERROR_MESSAGE);
				}
				else
				{
					ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
					String quickState = projectConfigurationPanel.quickState();
					Month startMonth = projectConfigurationPanel.getStartMonth();
					Month endMonth = projectConfigurationPanel.getEndMonth();
					_displayHelper.showDisplayFrames(quickState + ";Locs-" + chk.getText()
							+ ";Index-" + chk.getName(), baseScenario, alternatives, startMonth, endMonth);
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

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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collections;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.calgui.presentation.plotly.EpptPlotException;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.PlotConfigurationState;
import gov.ca.water.calgui.techservice.IDialogSvc;
import gov.ca.water.calgui.techservice.impl.DialogSvcImpl;
import gov.ca.water.calgui.project.EpptConfigurationController;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-06-2019
 */
class QuickResultsMouseListener extends MouseAdapter
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsMouseListener.class.getName());
	private final IDialogSvc _dialogSvc = DialogSvcImpl.getDialogSvcInstance();
	private final QuickResultsPanel _quickResultsPanel;
	private final DisplayHelper _displayHelper;
	private final EpptConfigurationController _epptConfigurationController;

	QuickResultsMouseListener(QuickResultsPanel quickResultsPanel, DisplayHelper displayHelper,
							  EpptConfigurationController epptConfigurationController)
	{
		_quickResultsPanel = quickResultsPanel;
		_displayHelper = displayHelper;
		_epptConfigurationController = epptConfigurationController;
	}

	@Override
	public void mouseClicked(MouseEvent e)
	{
		JComponent component = (JComponent) e.getComponent();
		String cName = component.getName();

		if(!SwingUtilities.isRightMouseButton(e) && e.isControlDown() && cName.startsWith("ckbp"))
		{
			// checkbox from quick results.
			JCheckBox chk = (JCheckBox) component;
			//This is to undo the click event action
			chk.setSelected(!chk.isSelected());
			Optional<EpptScenarioRun> baseScenario = _epptConfigurationController.getEpptScenarioBase();
			if(!baseScenario.isPresent())
			{
				_dialogSvc.getOK("Error - No Base Scenario defined", JOptionPane.ERROR_MESSAGE);
			}
			else
			{
				try
				{
					PlotConfigurationState plotConfigurationState = new PlotConfigurationStateBuilder(_quickResultsPanel.getSwingEngine())
							.createPlotConfigurationState();
					String name = chk.getName();
					GUILinksAllModelsBO guiLink = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance().getGuiLink(name);
					_displayHelper.showDisplayFramesGuiLink(plotConfigurationState, Collections.singletonList(guiLink));
				}
				catch(EpptPlotException ex)
				{
					LOGGER.log(Level.SEVERE, "Unable to plot Quick Results: " + chk.getText(), ex);
				}
			}
		}
	}
}

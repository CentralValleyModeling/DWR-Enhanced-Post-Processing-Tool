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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.event.MouseListener;
import javax.swing.*;
import javax.swing.border.LineBorder;

import gov.ca.water.calgui.presentation.DisplayHelper;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.calgui.project.EpptConfigurationController;
import org.apache.log4j.Logger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-21-2019
 */
public class QuickResultsPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanel.class.getName());
	private static final String LIST_REPORTS_ID = "lstReports";
	private static final String QUICK_RESULTS_XML_FILE = "Quick_Results.xml";

	private final DisplayHelper _displayHelper;

	public QuickResultsPanel(EpptConfigurationController epptConfigurationController)
	{
		try
		{
			_displayHelper = new DisplayHelper(this, epptConfigurationController);
			super.setLayout(new BorderLayout());
			Container swixmlQuickResultsPanel = renderSwixml(QUICK_RESULTS_XML_FILE);
//			super.add(swixmlQuickResultsPanel);
			Component reptabbedPane = getSwingEngine().find("reptabbedPane");
			setCheckBoxorMouseListener(reptabbedPane, new QuickResultsMouseListener(this, _displayHelper, epptConfigurationController));
			// Set up report list
			JList<?> lstReports = (JList<?>) getSwingEngine().find("lstReports");
			lstReports.setBorder(new LineBorder(Color.gray, 1));
			lstReports.setVisible(true);
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + QUICK_RESULTS_XML_FILE, e);
			throw new IllegalStateException(e);
		}
	}

	public DisplayHelper getDisplayHelper()
	{
		return _displayHelper;
	}

	@Override
	public String getJavaHelpId()
	{
		return "3.2_QuickResults.htm";
	}

	private void setCheckBoxorMouseListener(Component component, Object mouseListener)
	{
		if(component instanceof JCheckBox)
		{
			component.addMouseListener((MouseListener) mouseListener);
		}
		else if(component instanceof Container)
		{
			for(Component child : ((Container) component).getComponents())
			{
				setCheckBoxorMouseListener(child, mouseListener);
			}
		}
	}

	Component getReportsJList()
	{
		return getSwingEngine().find(LIST_REPORTS_ID);
	}

	JTabbedPane getVariables()
	{
		return ((JTabbedPane) getSwingEngine().find("variables"));
	}
}

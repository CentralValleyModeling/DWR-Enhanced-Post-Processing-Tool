/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

	public QuickResultsPanel()
	{
		try
		{
			_displayHelper = new DisplayHelper(this);
			super.setLayout(new BorderLayout());
			Container swixmlQuickResultsPanel = renderSwixml(QUICK_RESULTS_XML_FILE);
			super.add(swixmlQuickResultsPanel);
			Component reptabbedPane = getSwingEngine().find("reptabbedPane");
			setCheckBoxorMouseListener(reptabbedPane, new QuickResultsMouseListener(_displayHelper));
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
		return "Quick Results";
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

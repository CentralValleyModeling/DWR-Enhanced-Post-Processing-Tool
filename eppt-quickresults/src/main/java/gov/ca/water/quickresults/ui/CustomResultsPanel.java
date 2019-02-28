/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import java.awt.BorderLayout;
import java.awt.Container;
import javax.swing.*;

import gov.ca.water.calgui.presentation.WRIMSGUILinks;
import org.apache.log4j.Logger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-25-2019
 */
public class CustomResultsPanel extends EpptPanel
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanel.class.getName());
	private static final String LIST_REPORTS_ID = "lstReports";
	private static final String CUSTOM_RESULTS_XML_PATH = "ui/Custom_Results.xml";

	public CustomResultsPanel(JFrame frame)
	{
		try
		{
			super.setLayout(new BorderLayout());
			Container swixmlQuickResultsPanel = getSwingEngine().render(CUSTOM_RESULTS_XML_PATH);
			super.add(swixmlQuickResultsPanel);
			WRIMSGUILinks.buildWRIMSGUI(frame, (JPanel) getSwingEngine().find("WRIMS"));
		}
		catch(Exception e)
		{
			LOGGER.error("Error setting up quick results swing xml: " + CUSTOM_RESULTS_XML_PATH, e);
			throw new IllegalStateException(e);
		}
	}

}

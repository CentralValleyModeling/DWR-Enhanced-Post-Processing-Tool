/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import javax.swing.*;

import org.apache.log4j.Logger;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-26-2019
 */
class CustomResultsPanelTest
{
	private static Logger LOGGER = Logger.getLogger(CustomResultsPanelTest.class.getName());
	@Test
	void testCtor()
	{
		try
		{
			JFrame frame = new JFrame();
			CustomResultsPanel customResultsPanel = new CustomResultsPanel(frame);
			assertNotNull(customResultsPanel.getSwingEngine());
		}
		catch(Throwable e)
		{
			LOGGER.fatal(e);
		}
	}
}

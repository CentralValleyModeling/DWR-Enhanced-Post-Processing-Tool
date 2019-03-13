/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.quickresults;

import org.apache.log4j.Logger;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class QuickResultsPanelTest
{
	private static final Logger LOGGER = Logger.getLogger(QuickResultsPanelTest.class.getName());

	@Test
	public void testQuickResultsPanelCreation()
	{
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel();
		assertFalse(quickResultsPanel.getComponents().length == 0);
		assertNotNull(quickResultsPanel.getReportsJList());
		assertNotNull(quickResultsPanel.getSwingEngine());
		assertNotNull(quickResultsPanel.getVariables());
	}
}

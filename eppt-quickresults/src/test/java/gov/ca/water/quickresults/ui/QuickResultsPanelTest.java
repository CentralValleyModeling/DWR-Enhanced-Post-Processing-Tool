/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
class QuickResultsPanelTest
{
	@Test
	public void testQuickResultsPanelCreation()
	{
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel();
		assertFalse(quickResultsPanel.getComponents().length == 0);
	}
}

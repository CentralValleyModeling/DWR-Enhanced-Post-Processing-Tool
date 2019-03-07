/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.quickresults;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class QuickResultsListenerTest
{
	//	@Test
	public void testConstructor()
	{
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel();
		QuickResultsListener quickResultsListener = new QuickResultsListener(quickResultsPanel);
		assertNotNull(quickResultsListener);
	}
}

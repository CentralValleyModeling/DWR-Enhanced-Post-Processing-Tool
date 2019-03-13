/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.scenarioconfig;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class ScenarioConfigurationListenerTest
{
	//	@Test
	public void testConstructor()
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		ProjectConfigurationListener projectConfigurationListener = new ProjectConfigurationListener(
				projectConfigurationPanel);
		assertNotNull(projectConfigurationListener);
	}
}

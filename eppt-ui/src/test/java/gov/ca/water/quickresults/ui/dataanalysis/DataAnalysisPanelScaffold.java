/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.dataanalysis;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.EpptScaffold;
import gov.ca.water.calgui.project.EpptConfigurationController;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-26-2019
 */
public class DataAnalysisPanelScaffold extends EpptScaffold
{
	public static void main(String[] args) throws EpptInitializationException
	{
		new DataAnalysisPanelScaffold().initScaffold();
	}

	@Override
	protected EpptPanel buildEpptPanel()
	{
		EpptConfigurationController epptConfigurationController = new EpptConfigurationController();
		DataAnalysisPanel dataAnalysisPanel = new DataAnalysisPanel(epptConfigurationController);
		DataAnalysisListener dataAnalysisListener = new DataAnalysisListener(dataAnalysisPanel);
		dataAnalysisPanel.setActionListener(dataAnalysisListener);
		return dataAnalysisPanel;
	}
}

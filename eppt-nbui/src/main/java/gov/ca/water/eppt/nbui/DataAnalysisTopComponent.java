/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "DataAnalysisTopComponent",
		//iconBase="SET/PATH/TO/ICON/HERE",
		persistenceType = TopComponent.PERSISTENCE_ALWAYS
)
@TopComponent.Registration(mode = "editor", openAtStartup = true)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.DataAnalysisTopComponent")
@ActionReference(path = "Menu/Window" /*
 * , position = 333
 */)
@TopComponent.OpenActionRegistration(
		displayName = "#CTL_DataAnalysisAction",
		preferredID = "DataAnalysisTopComponent"
)
@Messages(
		{
				"CTL_DataAnalysisAction=DataAnalysis",
				"CTL_DataAnalysisTopComponent=DataAnalysis Window",
				"HINT_DataAnalysisTopComponent=This is a DataAnalysis window"
		})
public final class DataAnalysisTopComponent extends TopComponent
{

	public DataAnalysisTopComponent()
	{
		setName("Data Analysis");
	}
}

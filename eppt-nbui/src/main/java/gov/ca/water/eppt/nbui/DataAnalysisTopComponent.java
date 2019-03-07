/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.quickresults.ui.dataanalysis.DataAnalysisListener;
import gov.ca.water.quickresults.ui.dataanalysis.DataAnalysisPanel;
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
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 4444)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.DataAnalysisTopComponent")
@ActionReference(path = "Menu/Window", position = 4444)
@TopComponent.OpenActionRegistration(
		displayName = "Data Analysis",
		preferredID = "DataAnalysisTopComponent"
)
@Messages(
		{
				"CTL_DataAnalysisAction=Data Analysis",
				"CTL_DataAnalysisTopComponent=Data Analysis Window",
				"HINT_DataAnalysisTopComponent=This is the Data Analysis window"
		})
public final class DataAnalysisTopComponent extends TopComponent
{

	public DataAnalysisTopComponent()
	{
		setName("Data Analysis");
		DataAnalysisPanel dataAnalysisPanel = new DataAnalysisPanel();
		DataAnalysisListener dataAnalysisListener = new DataAnalysisListener(dataAnalysisPanel);
		dataAnalysisPanel.setActionListener(dataAnalysisListener);
		JScrollPane scrollPane = new JScrollPane(dataAnalysisPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}
}

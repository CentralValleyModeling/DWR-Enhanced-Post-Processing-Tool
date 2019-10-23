/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */
package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.quickresults.ui.dataanalysis.DataAnalysisListener;
import gov.ca.water.quickresults.ui.dataanalysis.DataAnalysisPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "DataAnalysisTopComponent",
		iconBase = "gov/ca/water/eppt/nbui/DataAnalysis.png"
)
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 5000)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.DataAnalysisTopComponent")
@ActionReferences({
		@ActionReference(path = "Menu/Window", position = 5000),
		@ActionReference(path = "Toolbars/Window", position = 5000)
})

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
public final class DataAnalysisTopComponent extends EpptTopComponent
{

	private final DataAnalysisPanel _dataAnalysisPanel;

	public DataAnalysisTopComponent()
	{
		setName("Data Analysis");
		_dataAnalysisPanel = new DataAnalysisPanel();
		DataAnalysisListener dataAnalysisListener = new DataAnalysisListener(_dataAnalysisPanel);
		_dataAnalysisPanel.setActionListener(dataAnalysisListener);
		JScrollPane scrollPane = new JScrollPane(_dataAnalysisPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

	@Override
	public String getJavaHelpId()
	{
		return _dataAnalysisPanel.getJavaHelpId();
	}
}

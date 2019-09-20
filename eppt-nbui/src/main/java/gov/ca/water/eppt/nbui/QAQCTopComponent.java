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

import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import gov.ca.water.quickresults.ui.report.QAQCReportPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "QAQCTopComponent",
		iconBase = "gov/ca/water/eppt/nbui/qaqc.png"
)
@TopComponent.Registration(mode = "editor", openAtStartup = true, position = 4444)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.QAQCTopComponent")
@ActionReferences({
		@ActionReference(path = "Menu/Window", position = 4444),
		@ActionReference(path = "Toolbars/Window", position = 4444)
})
@TopComponent.OpenActionRegistration(
		displayName = "QA/QC Report",
		preferredID = "QAQCTopComponent"
)
@Messages(
		{
				"CTL_QAQCAction=QA/QC Report",
				"CTL_QAQCTopComponent=QA/QC Report",
				"HINT_QAQCTopComponent=QA/QC Report Generation Dashboard"
		})
public final class QAQCTopComponent extends EpptTopComponent
{


	public QAQCTopComponent()
	{
		setName("QA/QC Report");
		QAQCReportPanel qaqcPanel = QAQCReportPanel.getInstance();
		JScrollPane scrollPane = new JScrollPane(qaqcPanel);
		setLayout(new BorderLayout());
		add(scrollPane, BorderLayout.CENTER);
	}

	@Override
	protected void componentOpened()
	{
		super.componentOpened();
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		QAQCReportPanel.getInstance().fillScenarioRuns(projectConfigurationPanel.getBaseScenario(),
				projectConfigurationPanel.getEpptScenarioAlternatives());
	}

	@Override
	public String getJavaHelpId()
	{
		return "4.5_QAQCReport.htm";
	}
}

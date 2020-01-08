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

package gov.ca.water.quickresults.ui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.quickresults.ui.report.WreslPanel;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-10-2019
 */
public class WreslRunDialog extends JDialog
{
	private final WreslPanel _wreslPanel;

	public WreslRunDialog(Frame frame)
	{
		super(frame, "Run WRESL Script", false);
		setSize(new Dimension(1200, 500));
		setLocationRelativeTo(frame);
		setLayout(new BorderLayout());
		_wreslPanel = new WreslPanel();
		add(_wreslPanel, BorderLayout.CENTER);
	}

	public void buildScenarioPanel(List<EpptScenarioRun> scenarioRuns)
	{
		_wreslPanel.buildScenarioPanel(scenarioRuns);
	}

	public void destroyProcesses()
	{
		_wreslPanel.destroyProcesses();
	}

	@Override
	public void dispose()
	{
		destroyProcesses();
		super.dispose();
	}
}

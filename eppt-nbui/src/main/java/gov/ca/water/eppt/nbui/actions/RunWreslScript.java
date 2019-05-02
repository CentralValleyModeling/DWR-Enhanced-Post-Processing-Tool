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

package gov.ca.water.eppt.nbui.actions;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.time.LocalDate;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.wresl.WreslScriptException;
import gov.ca.water.calgui.wresl.WreslScriptRunner;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import org.jfree.data.time.Month;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-18-2019
 */
@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.RunWreslScript"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/run.png",
		displayName = "Run WRESL Script"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/Tools", position = 0)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 666)
		})
@NbBundle.Messages("CTL_RunWreslScript=Run WRESL Script")
public class RunWreslScript implements ActionListener
{
	private static final Logger LOGGER = Logger.getLogger(RunWreslScript.class.getName());

	@Override
	public void actionPerformed(ActionEvent e)
	{
		System.getenv("temp_wrims2");
		try
		{
			initReport(
					new String[]{"-config=J:\\DWR\\CalLiteBuild\\CalLiteGUI_P3\\Scenarios\\Run_Details\\DEFAULT\\DEFAULT.config"});
		}
		catch(WreslScriptException e1)
		{
			LOGGER.log(Level.SEVERE, "Error in WRESL Script Run");
		}
	}

	void initReport(String[] args) throws WreslScriptException
	{
		ProjectConfigurationPanel projectConfigurationPanel = ProjectConfigurationPanel.getProjectConfigurationPanel();
		Month startMonth = projectConfigurationPanel.getStartMonth();
		Month endMonth = projectConfigurationPanel.getEndMonth();
		LocalDate start = LocalDate.of(startMonth.getYearValue(), startMonth.getMonth(), 0);
		LocalDate end = LocalDate.of(endMonth.getYearValue(), endMonth.getMonth(), 0);
		List<EpptScenarioRun> epptScenarioRuns = projectConfigurationPanel.getEpptScenarioRuns();
		for(EpptScenarioRun scenarioRun : epptScenarioRuns)
		{
			WreslScriptRunner wreslScriptRunner = new WreslScriptRunner(scenarioRun);
			wreslScriptRunner.run(start, end);
		}
	}

}

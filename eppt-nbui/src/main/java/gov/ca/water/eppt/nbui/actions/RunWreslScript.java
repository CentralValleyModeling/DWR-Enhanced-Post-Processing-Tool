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
import java.util.List;

import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.eppt.nbui.EpptControllerProvider;
import gov.ca.water.quickresults.ui.WreslRunDialog;
import gov.ca.water.calgui.project.EpptConfigurationController;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle;
import org.openide.windows.WindowManager;

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

	private static WreslRunDialog wreslRunDialog;

	@Override
	public void actionPerformed(ActionEvent e)
	{
		action();
	}

	private static void action()
	{
		EpptConfigurationController epptConfigurationController = EpptControllerProvider.getEpptConfigurationController();
		if(wreslRunDialog == null)
		{
			wreslRunDialog = new WreslRunDialog(WindowManager.getDefault().getMainWindow(), epptConfigurationController);
		}
		List<EpptScenarioRun> allEpptScenarioRuns = epptConfigurationController.getScenarioRuns();
		wreslRunDialog.buildScenarioPanel(allEpptScenarioRuns);
		wreslRunDialog.revalidate();
		wreslRunDialog.setVisible(true);
		wreslRunDialog.toFront();
	}

	public static void destroyProcesses()
	{
		if(wreslRunDialog != null)
		{
			wreslRunDialog.destroyProcesses();
		}
	}

}

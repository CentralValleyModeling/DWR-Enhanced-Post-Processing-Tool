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

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.nio.file.Path;
import java.nio.file.Paths;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.quickresults.ui.projectconfig.ProjectConfigurationPanel;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.WindowManager;

import hec.dssgui.ListSelection;

@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.DssVueAction"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/HecDssVue.png",
		displayName = "HEC-DSSVue"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/Tools", position = 1111, separatorAfter = 1112)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 555)
		})
@Messages("CTL_DssVueAction=HEC-DSSVue")
public final class DssVueAction implements ActionListener
{

	private ListSelection _listSelection;

	@Override
	public void actionPerformed(ActionEvent e)
	{
		if(_listSelection == null)
		{
			_listSelection = createListSelection();
			Frame mainWindow = WindowManager.getDefault().getMainWindow();
			_listSelection.setLocationRelativeTo(mainWindow);
			_listSelection.setDirectory(EpptPreferences.getScenariosPaths().toString());
		}

		Path selectedDssPath = ProjectConfigurationPanel.getProjectConfigurationPanel().getSelectedDssPath();
		if(selectedDssPath != null)
		{
			boolean fileAlreadyOpen = _listSelection.getDssFilenames()
													.stream()
													.map(Object::toString)
													.map(p -> Paths.get(p.toString()))
													.anyMatch(p -> p.equals(selectedDssPath));
			if(!fileAlreadyOpen)
			{
				_listSelection.open(selectedDssPath.toString());
			}

		}
		_listSelection.setVisible(true);
	}

	private ListSelection createListSelection()
	{
		boolean useTabbedPane = true;
		boolean exitOnClose = false;
		boolean clientServer = false;
		int mode = ListSelection.FULL_FUNCTION;
		return new ListSelection("HEC-DSSVue", mode, useTabbedPane, exitOnClose, clientServer);
	}
}

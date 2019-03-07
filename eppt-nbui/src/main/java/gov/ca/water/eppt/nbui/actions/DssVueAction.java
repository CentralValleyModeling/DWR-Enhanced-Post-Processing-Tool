/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui.actions;

import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.WindowManager;

import hec.dssgui.ListSelection;

@ActionID(
		category = "Tools",
		id = "gov.ca.water.eppt.nbui.actions.DssVueAction"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/HecDssVue.png",
		displayName = "HEC-DSSVue"
)
@ActionReference(path = "Menu/Tools", position = 0, separatorAfter = 50)
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

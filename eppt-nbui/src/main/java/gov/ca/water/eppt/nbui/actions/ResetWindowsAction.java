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
import javax.swing.*;

import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionRegistration;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-21-2019
 */
@ActionID(
		category = "Window",
		id = "gov.ca.water.eppt.nbui.actions.ResetWindowsAction"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/ResetWindows.png",
		displayName = "Reset Windows"
)
@ActionReference(path = "Toolbars/Window", position = 10_000)
@NbBundle.Messages("CTL_ResetWindowsAction=Reset Windows")
public class ResetWindowsAction implements ActionListener
{

	@Override
	public void actionPerformed(ActionEvent e)
	{
		Action resetWindowsAction = FileUtil.getConfigObject(
				"Actions/Window/org-netbeans-core-windows-actions-ResetWindowsAction.instance", Action.class);
		resetWindowsAction.actionPerformed(e);
	}
}

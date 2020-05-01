/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
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
import javax.swing.*;

import gov.ca.water.calgui.constant.EpptPreferences;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle.Messages;
import org.openide.util.actions.Presenter;

@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.ToggleAutoRefreshAction"
)
@ActionRegistration(
		displayName = "Auto Refresh Trend Report"
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/Tools", position = 2)
		})
@Messages("CTL_ToggleAutoRefreshAction=Auto Refresh Trend Report")
public final class ToggleAutoRefreshAction extends AbstractAction implements Presenter.Menu
{

	@Override
	public void actionPerformed(ActionEvent e)
	{
		EpptPreferences.setAutoRefreshTrendReport(!EpptPreferences.getAutoRefreshTrendReport());
	}

	@Override
	public JMenuItem getMenuPresenter()
	{
		JCheckBoxMenuItem jCheckBoxMenuItem = new JCheckBoxMenuItem("Auto Refresh Trend Report");
		jCheckBoxMenuItem.setSelected(EpptPreferences.getAutoRefreshTrendReport());
		jCheckBoxMenuItem.addActionListener(this);
		return jCheckBoxMenuItem;
	}
}

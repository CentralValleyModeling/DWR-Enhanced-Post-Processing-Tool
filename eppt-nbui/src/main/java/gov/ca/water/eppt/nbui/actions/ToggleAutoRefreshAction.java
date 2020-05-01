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

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.eppt.nbui.EpptControllerProvider;
import gov.ca.water.eppt.nbui.Installer;
import gov.ca.water.eppt.nbui.ProjectConfigurationTopComponent;
import gov.ca.water.eppt.nbui.projectconfig.ProjectConfigurationIO;
import org.netbeans.api.actions.Savable;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.awt.DropDownButtonFactory;
import org.openide.util.ContextAwareAction;
import org.openide.util.Lookup;
import org.openide.util.NbBundle.Messages;
import org.openide.util.Utilities;
import org.openide.util.actions.Presenter;
import org.openide.windows.Mode;
import org.openide.windows.WindowManager;

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

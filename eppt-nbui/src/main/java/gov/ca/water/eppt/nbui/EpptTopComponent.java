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

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import org.openide.windows.Mode;
import org.openide.windows.TopComponent;
import org.openide.windows.WindowManager;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-07-2019
 */
public abstract class EpptTopComponent extends TopComponent
{
	private static final Logger LOGGER = Logger.getLogger(EpptTopComponent.class.getName());
	public abstract String getJavaHelpId();

	@Override
	public Action[] getActions()
	{
		Action[] actions = super.getActions();
		List<Action> actionList = new ArrayList<>(Arrays.asList(actions));
		Action openHelp = new AbstractAction("Help...")
		{
			@Override
			public void actionPerformed(ActionEvent evt)
			{
				EpptHelpTopComponent epptHelpTopComponent = getOpenedEpptHelpTopComponent();
				if(epptHelpTopComponent == null)
				{
					epptHelpTopComponent = createNewHelpTopComponent();
				}
				if(epptHelpTopComponent != null)
				{
					epptHelpTopComponent.selectCurrentHelperId(getJavaHelpId());
					epptHelpTopComponent.requestActive();
				}
			}
		};
		actionList.add(0, null);
		actionList.add(0, openHelp);
		return actionList.toArray(new Action[0]);
	}

	private EpptHelpTopComponent createNewHelpTopComponent()
	{
		EpptHelpTopComponent epptHelpTopComponent = null;
		try
		{
			epptHelpTopComponent = new EpptHelpTopComponent();
			Mode properties = WindowManager.getDefault().findMode("properties");
			properties.dockInto(epptHelpTopComponent);
			epptHelpTopComponent.open();
		}
		catch(EpptInitializationException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to open EPPT Help Top Component", ex);
		}
		return epptHelpTopComponent;
	}

	private EpptHelpTopComponent getOpenedEpptHelpTopComponent()
	{
		EpptHelpTopComponent epptHelpTopComponent = null;
		Set<? extends Mode> modes = WindowManager.getDefault().getModes();
		for(Mode mode : modes)
		{
			TopComponent[] topComponents = mode.getTopComponents();
			for(TopComponent tc : topComponents)
			{
				if(tc instanceof EpptHelpTopComponent)
				{
					epptHelpTopComponent = (EpptHelpTopComponent) tc;
					epptHelpTopComponent.open();
					break;
				}
			}
		}
		return epptHelpTopComponent;
	}
}

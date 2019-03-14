/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui.actions;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.awt.ActionRegistration;
import org.openide.util.NbBundle;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-13-2019
 */
@ActionID(
		category = "EPPT",
		id = "gov.ca.water.eppt.nbui.actions.NewProjectConfiguration"
)
@ActionRegistration(
		iconBase = "gov/ca/water/eppt/nbui/actions/new.png",
		displayName = "New..."
)
@ActionReferences(
		{
				@ActionReference(path = "Menu/File", position = 0)
				,
				@ActionReference(path = "Toolbars/EPPT", position = 0)
		})
@NbBundle.Messages("CTL_NewProjectConfiguration=New...")
public class NewProjectConfiguration implements ActionListener
{

	private static final Logger LOGGER = Logger.getLogger(NewProjectConfiguration.class.getName());

	@Override
	public void actionPerformed(ActionEvent e)
	{
		try
		{
			new SaveAsProjectConfiguration().saveAs();
		}
		catch(IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Error saving Project Configuration", ex);
		}
	}
}

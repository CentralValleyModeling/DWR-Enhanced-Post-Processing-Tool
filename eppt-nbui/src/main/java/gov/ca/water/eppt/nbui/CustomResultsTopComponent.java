/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package gov.ca.water.eppt.nbui;

import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.util.NbBundle.Messages;
import org.openide.windows.TopComponent;

/**
 * Top component which displays something.
 */
@TopComponent.Description(
		preferredID = "CustomResultsTopComponent",
		//iconBase="SET/PATH/TO/ICON/HERE",
		persistenceType = TopComponent.PERSISTENCE_ALWAYS
)
@TopComponent.Registration(mode = "editor", openAtStartup = true)
@ActionID(category = "Window", id = "gov.ca.water.eppt.nbui.CustomResultsTopComponent")
@ActionReference(path = "Menu/Window" /*
 * , position = 333
 */)
@TopComponent.OpenActionRegistration(
		displayName = "#CTL_CustomResultsAction",
		preferredID = "CustomResultsTopComponent"
)
@Messages(
		{
				"CTL_CustomResultsAction=CustomResults",
				"CTL_CustomResultsTopComponent=CustomResults Window",
				"HINT_CustomResultsTopComponent=This is a CustomResults window"
		})
public final class CustomResultsTopComponent extends TopComponent
{

	public CustomResultsTopComponent()
	{
		setName("Custom Results");
	}
}

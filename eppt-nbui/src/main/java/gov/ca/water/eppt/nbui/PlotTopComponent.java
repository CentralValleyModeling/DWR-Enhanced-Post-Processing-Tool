/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.eppt.nbui;

import java.awt.BorderLayout;
import javax.swing.*;

import org.openide.windows.TopComponent;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-06-2019
 */
public class PlotTopComponent extends TopComponent
{
	public PlotTopComponent(JTabbedPane tabbedPane)
	{
		tabbedPane.setSelectedIndex(0);
		JScrollPane jScrollPane = new JScrollPane();
		jScrollPane.setViewportView(tabbedPane);
		super.setLayout(new BorderLayout());
		super.add(jScrollPane, BorderLayout.CENTER);
		super.setName("CalLite Results - " + tabbedPane.getName());
	}

	@Override
	public int getPersistenceType()
	{
		return TopComponent.PERSISTENCE_NEVER;
	}

	@Override
	protected String preferredID()
	{
		return "PlotTopComponent";
	}
}

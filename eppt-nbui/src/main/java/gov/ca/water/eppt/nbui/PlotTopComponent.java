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

import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.*;

import org.openide.windows.TopComponent;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-06-2019
 */
public class PlotTopComponent extends EpptTopComponent
{
	PlotTopComponent(JTabbedPane tabbedPane)
	{
		tabbedPane.setPreferredSize(new Dimension(545, 630));
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

	@Override
	public String getJavaHelpId()
	{
		return "Viewing Results";
	}
}

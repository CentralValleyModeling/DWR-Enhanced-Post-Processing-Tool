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
import java.awt.Component;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.presentation.plotly.PlotlyPane;
import org.openide.windows.TopComponent;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-06-2019
 */
public class PlotTopComponent extends EpptTopComponent
{
	private static final Logger LOGGER = Logger.getLogger(PlotTopComponent.class.getName());
	private final JTabbedPane _tabbedPane;
	private boolean _initialized;

	public PlotTopComponent()
	{
		_tabbedPane = null;
		LOGGER.log(Level.FINEST, "NOOP constructor for Netbeans serialization routine (even though this top component is not persistent");
	}

	PlotTopComponent(JTabbedPane tabbedPane)
	{
		_tabbedPane = tabbedPane;
		int tabCount = tabbedPane.getTabCount();
		for(int i = 0; i < tabCount; i++)
		{
			Component tabComponentAt = tabbedPane.getComponentAt(i);
			String titleAt = tabbedPane.getTitleAt(i);
			tabbedPane.removeTabAt(i);
			JScrollPane scrollPane = new JScrollPane();
			scrollPane.setViewportView(tabComponentAt);
			tabbedPane.insertTab(titleAt, null, scrollPane, null, i);
		}
		super.setLayout(new BorderLayout());
		super.add(tabbedPane, BorderLayout.CENTER);
		super.setName("Results - " + tabbedPane.getName());
	}

	@Override
	protected void componentActivated()
	{
		super.componentActivated();
		if(!_initialized)
		{
			_initialized = true;
			_tabbedPane.addChangeListener(e -> loadSelectedPlot());
			_tabbedPane.setSelectedIndex(0);
			loadSelectedPlot();
		}
	}

	private void loadSelectedPlot()
	{
		Component selectedComponent = _tabbedPane.getSelectedComponent();
		if(selectedComponent instanceof JScrollPane)
		{
			Component component = ((JScrollPane) selectedComponent).getViewport().getComponents()[0];
			if(component instanceof PlotlyPane)
			{
				((PlotlyPane) component).initPlot();
			}
		}
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
		return "4.2_ViewingResults.htm";
	}
}

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

package gov.ca.water.quickresults.ui.dashboard;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.customresults.CustomResultsScaffold;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-21-2019
 */
public class DashboardScaffold
{
	public final void initScaffold() throws EpptInitializationException
	{

		try
		{
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch(ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e)
		{
			fail(e);
		}
		DashboardPanel epptPanel = new DashboardPanel();
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			jFrame.setLayout(new BorderLayout());
			jFrame.add(epptPanel, BorderLayout.CENTER);
			//			jFrame.add(buildUiManagerPanel(jFrame), BorderLayout.NORTH);
			jFrame.setSize(1000,700);
			jFrame.setBackground(Color.WHITE);
			jFrame.setVisible(true);
		});
	}

	public static void main(String[] args) throws EpptInitializationException
	{
		new DashboardScaffold().initScaffold();
	}
}

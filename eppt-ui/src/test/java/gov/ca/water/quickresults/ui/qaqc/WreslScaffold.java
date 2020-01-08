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

package gov.ca.water.quickresults.ui.qaqc;

import java.awt.BorderLayout;
import java.awt.Color;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.quickresults.ui.report.WreslPanel;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-21-2019
 */
public class WreslScaffold
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
		WreslPanel epptPanel = new WreslPanel();
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			jFrame.setLayout(new BorderLayout());
			jFrame.add(epptPanel, BorderLayout.CENTER);
			jFrame.setSize(1000,700);
			jFrame.setBackground(Color.WHITE);
			jFrame.setVisible(true);
		});
	}

	public static void main(String[] args) throws EpptInitializationException
	{
		new WreslScaffold().initScaffold();
	}
}

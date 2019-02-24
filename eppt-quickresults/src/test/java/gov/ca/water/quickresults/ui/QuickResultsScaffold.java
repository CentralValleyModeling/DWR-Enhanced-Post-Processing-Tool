/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui;

import java.awt.BorderLayout;
import javax.swing.*;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class QuickResultsScaffold
{

	public static void main(String[] args)
	{
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel();
		ScenarioConfigurationPanel scenarioConfigurationPanel1 = new ScenarioConfigurationPanel();
		QuickResultsListener quickResultsListener = new QuickResultsListener(quickResultsPanel,
				scenarioConfigurationPanel1);
		quickResultsPanel.getSwingEngine().setActionListener(quickResultsPanel, quickResultsListener);
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			try
			{
				UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
			}
			catch(ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e)
			{
				fail(e);
			}
			jFrame.setLayout(new BorderLayout());
			jFrame.add(quickResultsPanel, BorderLayout.CENTER);
			jFrame.pack();
			jFrame.setVisible(true);
		});
	}
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.customresults;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationPanel;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-26-2019
 */
public class CustomResultsScaffold
{
	public static void main(String[] args)
	{
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		CustomResultsPanel customResultsPanel = new CustomResultsPanel();
		customResultsPanel.getSwingEngine().setActionListener(customResultsPanel,
				new CustomResultsListener(customResultsPanel));
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
			ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
			jFrame.setLayout(new BorderLayout());
			jFrame.add(customResultsPanel, BorderLayout.CENTER);
			jFrame.pack();
			jFrame.setVisible(true);
		});
	}
}

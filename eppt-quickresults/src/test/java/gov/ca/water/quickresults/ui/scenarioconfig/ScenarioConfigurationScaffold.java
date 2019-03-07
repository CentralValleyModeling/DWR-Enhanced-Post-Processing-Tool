/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.scenarioconfig;

import java.awt.BorderLayout;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bus_service.impl.GuiLinksSeedDataSvcImpl;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class ScenarioConfigurationScaffold
{


	public static void main(String[] args) throws EpptInitializationException
	{
		try
		{
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch(ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e)
		{
			fail(e);
		}
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
		ScenarioConfigurationListener scenarioConfigurationListener = new ScenarioConfigurationListener(
				scenarioConfigurationPanel);
		scenarioConfigurationPanel.getSwingEngine().setActionListener(scenarioConfigurationPanel,
				scenarioConfigurationListener);
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			jFrame.setLayout(new BorderLayout());
			jFrame.add(scenarioConfigurationPanel, BorderLayout.CENTER);
			jFrame.pack();
			jFrame.setVisible(true);
		});
	}
}

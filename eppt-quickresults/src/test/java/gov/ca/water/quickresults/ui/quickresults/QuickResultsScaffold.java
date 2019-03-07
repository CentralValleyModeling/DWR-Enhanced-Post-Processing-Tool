/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.quickresults.ui.quickresults;

import java.awt.BorderLayout;
import java.awt.Component;
import java.util.List;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.RBListItemBO;
import gov.ca.water.calgui.bus_service.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.quickresults.ui.scenarioconfig.ScenarioConfigurationPanel;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class QuickResultsScaffold
{

	public static void main(String[] args) throws EpptInitializationException
	{
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		QuickResultsPanel quickResultsPanel = new QuickResultsPanel();
		QuickResultsListener quickResultsListener = new QuickResultsListener(quickResultsPanel);
		quickResultsPanel.getSwingEngine().setActionListener(quickResultsPanel, quickResultsListener);
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			addScenarios();
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


	private static void addScenarios()
	{
		ScenarioConfigurationPanel scenarioConfigurationPanel = ScenarioConfigurationPanel.getScenarioConfigurationPanel();
		Component component = scenarioConfigurationPanel.getSwingEngine().find("SelectedList");
		if(component instanceof JList)
		{
			JList<RBListItemBO> lstScenarios = (JList<RBListItemBO>) component;
			List<RBListItemBO> currentScenarios = scenarioConfigurationPanel.getScenarios();
			String baseFile = Thread.currentThread().getContextClassLoader().getResource(
					"Base.dss").getFile().substring(1);
			RBListItemBO base = new RBListItemBO(baseFile, "Base.dss");
			base.setSelected(true);
			currentScenarios.add(base);
			String altFile = Thread.currentThread().getContextClassLoader().getResource(
					"Alternative.dss").getFile().substring(1);
			currentScenarios.add(new RBListItemBO(altFile, "Alternative.dss"));
			DefaultListModel<RBListItemBO> defaultModel = new DefaultListModel<>();
			for(RBListItemBO item : currentScenarios)
			{
				defaultModel.addElement(item);
			}
			lstScenarios.setModel(defaultModel);
		}
	}
}

/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.BorderLayout;
import java.nio.file.Paths;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.quickresults.ui.projectconfig.scenarioconfig.ScenarioRunEditor;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 04-08-2019
 */
public class ScenarioRunEditorScaffold
{
	private static Logger LOGGER = Logger.getLogger(ScenarioRunEditorScaffold.class.getName());

	public static void main(String[] args) throws EpptInitializationException
	{
		String userDir = Paths.get(System.getProperty("user.dir")).resolve(
				"target/test-classes").toAbsolutePath().toString();
		System.setProperty("user.dir", userDir);
		try
		{
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		}
		catch(ClassNotFoundException | InstantiationException | IllegalAccessException | UnsupportedLookAndFeelException e)
		{
			return;
		}
		GuiLinksSeedDataSvcImpl.createSeedDataSvcImplInstance();
		WaterYearDefinitionSvc.createSeedDataSvcImplInstance();
		JFrame jFrame = new JFrame();
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		SwingUtilities.invokeLater(() ->
		{
			jFrame.setLayout(new BorderLayout());
			jFrame.pack();
			jFrame.setVisible(true);
			SwingUtilities.invokeLater(() ->
			{
				EpptConfigurationController epptConfigurationController = new EpptConfigurationController();
				ScenarioRunEditor scenarioRunEditor = new ScenarioRunEditor(jFrame, epptConfigurationController.getScenarioRuns());
				scenarioRunEditor.setVisible(true);
				EpptScenarioRun run = scenarioRunEditor.createRun();
				if(run == null)
				{
					LOGGER.log(Level.INFO, "No run created");
				}
				else
				{
					LOGGER.log(Level.INFO, "Name: {0}", new Object[]{run.getName()});
					LOGGER.log(Level.INFO, "Description: {0}", new Object[]{run.getDescription()});
					LOGGER.log(Level.INFO, "Model: {0}", new Object[]{run.getModel()});
					LOGGER.log(Level.INFO, "Output Path: {0}", new Object[]{run.getOutputPath()});
					LOGGER.log(Level.INFO, "WRESL Main: {0}", new Object[]{run.getWreslDirectory()});
				}
				jFrame.setVisible(false);
				jFrame.dispose();
			});
		});
	}
}

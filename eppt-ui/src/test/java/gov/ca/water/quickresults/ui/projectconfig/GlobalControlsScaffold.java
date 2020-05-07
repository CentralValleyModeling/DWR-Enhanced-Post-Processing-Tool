/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.quickresults.ui.projectconfig;

import java.awt.BorderLayout;
import java.awt.Dimension;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.quickresults.ui.EpptPanel;
import gov.ca.water.quickresults.ui.EpptScaffold;
import gov.ca.water.quickresults.ui.global.EpptConfigurationPane;
import gov.ca.water.quickresults.ui.global.PlotConfigurationPane;
import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-23-2019
 */
public class GlobalControlsScaffold extends EpptScaffold
{


	public static void main(String[] args) throws EpptInitializationException
	{
		new GlobalControlsScaffold().initScaffold();
	}

	@Override
	protected EpptPanel buildEpptPanel()
	{
		EpptPanel epptPanel = new EpptPanel()
		{

			@Override
			public String getJavaHelpId()
			{
				return "";
			}
		};
		epptPanel.setLayout(new BorderLayout());
		JFXPanel jfxPanel = new JFXPanel();
		epptPanel.add(jfxPanel, BorderLayout.CENTER);
		EpptConfigurationController epptConfigurationController = new EpptConfigurationController();
		Platform.runLater(()->
		{
			PlotConfigurationPane epptConfigurationPane = new PlotConfigurationPane(epptConfigurationController);
			jfxPanel.setScene(new Scene(epptConfigurationPane));
		});
		epptPanel.setPreferredSize(new Dimension(400, 900));
		return epptPanel;
	}
}

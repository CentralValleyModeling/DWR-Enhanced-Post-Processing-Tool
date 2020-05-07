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

package gov.ca.water.quickresults.ui.global;

import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.quickresults.ui.projectconfig.scenariotable.ScenarioTablePane;
import javafx.application.Platform;
import javafx.scene.Node;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.TilePane;
import javafx.scene.layout.VBox;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-25-2020
 */
public class EpptConfigurationPane extends BorderPane
{
	private final EpptProjectPane _projectPane;
	private final ScenarioTablePane _scenarioPane;

	public EpptConfigurationPane(EpptConfigurationController controller)
	{
		_projectPane = new EpptProjectPane(controller);
		_scenarioPane = new ScenarioTablePane(controller);
		initComponents();
	}

	private void initComponents()
	{
		VBox gridPane = new VBox();
		gridPane.getChildren().add(_projectPane);
		gridPane.getChildren().add(_scenarioPane);
		setCenter(gridPane);
		setStyle("-fx-font-size: 11");
	}

	public void reloadProject()
	{
		_projectPane.reloadProject();
		_scenarioPane.reloadProject();
	}
}

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
	private final EpptTimeWindowPane _timeWindowPane;
	private final EpptMonthlyPeriodPane _epptMonthlyPeriodPane;
	private final EpptAnnualPeriodPane _epptAnnualPeriodPane;
	private final EpptStatisticsPane _epptStatisticsPane;

	public EpptConfigurationPane(EpptConfigurationController controller)
	{
		_projectPane = new EpptProjectPane(controller);
		_scenarioPane = new ScenarioTablePane(controller);
		_timeWindowPane = new EpptTimeWindowPane(controller);
		_epptMonthlyPeriodPane = new EpptMonthlyPeriodPane(controller);
		_epptAnnualPeriodPane = new EpptAnnualPeriodPane(controller);
		_epptStatisticsPane = new EpptStatisticsPane(controller);
		initComponents();
	}

	private void initComponents()
	{
		VBox gridPane = new VBox();
		gridPane.getChildren().add(_projectPane);
		gridPane.getChildren().add(_scenarioPane);
		gridPane.getChildren().add(_timeWindowPane);
		Node filterControls = buildFilterControls();
		gridPane.getChildren().add(filterControls);
		gridPane.getChildren().add(_epptStatisticsPane);
		setCenter(gridPane);
		setStyle("-fx-font-size: 11");
	}

	private Node buildFilterControls()
	{
		BorderPane borderPane = new BorderPane();
		_epptAnnualPeriodPane.expandedProperty().bindBidirectional(_epptMonthlyPeriodPane.expandedProperty());
		GridPane gridPane = new GridPane();
		gridPane.add(_epptMonthlyPeriodPane, 1, 1);
		gridPane.add(_epptAnnualPeriodPane, 2, 1);
		GridPane.setHgrow(_epptAnnualPeriodPane, Priority.ALWAYS);
		borderPane.setCenter(gridPane);
		borderPane.setPrefHeight(320);
		return borderPane;
	}

	public void reloadProject()
	{
		_projectPane.reloadProject();
		_scenarioPane.reloadProject();
		_timeWindowPane.reloadProject();
		_epptMonthlyPeriodPane.reloadProject();
		_epptAnnualPeriodPane.reloadProject();
		_epptStatisticsPane.reloadProject();
	}
}

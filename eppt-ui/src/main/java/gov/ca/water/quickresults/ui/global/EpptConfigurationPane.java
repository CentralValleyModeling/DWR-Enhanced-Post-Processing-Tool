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

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-25-2020
 */
public class EpptConfigurationPane extends BorderPane
{
	private final EpptConfigurationController _controller;
	private final EpptProjectPane _projectPane;
	private final ScenarioTablePane _scenarioPane;
	private final EpptTimeWindowPane _timeWindowPane;
	private final EpptMonthlyPeriodPane _epptMonthlyPeriodPane;
	private final EpptAnnualPeriodPane _epptAnnualPeriodPane;
	private final EpptStatisticsPane _epptStatisticsPane;

	public EpptConfigurationPane(EpptConfigurationController controller)
	{
		_controller = controller;
		_projectPane = new EpptProjectPane(_controller);
		_scenarioPane = new ScenarioTablePane(_controller);
		_timeWindowPane = new EpptTimeWindowPane(_controller);
		_epptMonthlyPeriodPane = new EpptMonthlyPeriodPane(_controller);
		_epptAnnualPeriodPane = new EpptAnnualPeriodPane(_controller);
		_epptStatisticsPane = new EpptStatisticsPane(_controller);
		initComponents();
	}

	private void initComponents()
	{
		GridPane gridPane = new GridPane();
		gridPane.add(_projectPane, 0, 0);
		gridPane.add(_scenarioPane, 0, 1);
		gridPane.add(_timeWindowPane, 0, 2);
		gridPane.add(buildFilterControls(), 0, 3);
		gridPane.add(_epptStatisticsPane, 0, 4);
		GridPane.setHgrow(_timeWindowPane, Priority.ALWAYS);

		setCenter(gridPane);
		setPrefSize(570, 900);
	}

	private Node buildFilterControls()
	{
		_epptAnnualPeriodPane.expandedProperty().bindBidirectional(_epptMonthlyPeriodPane.expandedProperty());
		BorderPane borderPane = new BorderPane();
		borderPane.setLeft(_epptMonthlyPeriodPane);
		borderPane.setCenter(_epptAnnualPeriodPane);
		GridPane.setVgrow(borderPane, Priority.ALWAYS);
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

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

	public EpptConfigurationPane(EpptConfigurationController controller)
	{
		_controller = controller;
		_projectPane = new EpptProjectPane(_controller);
		_scenarioPane = new ScenarioTablePane(_controller);
		_timeWindowPane = new EpptTimeWindowPane(_controller);
		initComponents();
	}

	private void initComponents()
	{
		GridPane gridPane = new GridPane();
		gridPane.add(_projectPane, 0, 0);
		gridPane.add(_scenarioPane, 0, 1);
		gridPane.add(_timeWindowPane, 0, 2);
		gridPane.add(buildFilterControls(), 0, 3);
		gridPane.add(new EpptStatisticsPane(_controller), 0, 4);
		GridPane.setHgrow(_timeWindowPane, Priority.ALWAYS);

		setCenter(gridPane);
		setPrefSize(570, 900);
	}

	private Node buildFilterControls()
	{
		EpptMonthlyPeriodPane epptMonthlyPeriodPane = new EpptMonthlyPeriodPane(_controller);
		EpptAnnualPeriodPane epptAnnualPeriodPane = new EpptAnnualPeriodPane(_controller);
		epptAnnualPeriodPane.expandedProperty().bindBidirectional(epptMonthlyPeriodPane.expandedProperty());
		BorderPane borderPane = new BorderPane();
		borderPane.setLeft(epptMonthlyPeriodPane);
		borderPane.setCenter(epptAnnualPeriodPane);
		return borderPane;
	}

	public void reloadProject()
	{
		_projectPane.reloadProject();
		_scenarioPane.reloadProject();
		_timeWindowPane.reloadProject();
	}
}

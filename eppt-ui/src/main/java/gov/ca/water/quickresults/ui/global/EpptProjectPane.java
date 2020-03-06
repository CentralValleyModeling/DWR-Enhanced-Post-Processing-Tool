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
import javafx.geometry.Insets;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-25-2020
 */
class EpptProjectPane extends TitledPane
{
	private final TextField _projectNameField = new TextField();
	private final TextField _projectDescriptionField = new TextField();
	private final EpptConfigurationController _controller;

	EpptProjectPane(EpptConfigurationController controller)
	{
		_controller = controller;
		setExpanded(false);
		initComponents();
		fillProject(_controller.getProjectName(), _controller.getProjectDescription());
		initListeners();
	}

	private void initListeners()
	{
		_projectDescriptionField.textProperty().addListener((e,o,n)->
		{
			_controller.setProjectDescription(n);
			_controller.setModified();
		});
		_projectNameField.textProperty().addListener((e,o,n)->
		{
			_controller.setProjectName(n);
			_controller.setModified();
		});
	}

	private void initComponents()
	{
		setGraphicTextGap(0);
		BorderPane borderPane = new BorderPane();
		Label statisticLabel = new Label("Project Info");
		borderPane.setLeft(statisticLabel);
		BorderPane.setMargin(statisticLabel, new Insets(5));
		setGraphic(borderPane);
		GridPane gridPane = new GridPane();
		gridPane.setHgap(5.0);
		gridPane.setVgap(5.0);
		gridPane.add(new Label("Project Name: "), 0, 0);
		gridPane.add(new Label("Description: "), 0, 1);
		_projectNameField.setMaxWidth(Double.MAX_VALUE);
		_projectDescriptionField.setMaxWidth(Double.MAX_VALUE);
		gridPane.add(_projectNameField, 1, 0);
		gridPane.add(_projectDescriptionField, 1, 1);
		GridPane.setHgrow(_projectDescriptionField, Priority.ALWAYS);
		BorderPane mainPane = new BorderPane();
		mainPane.setCenter(gridPane);
		setContent(mainPane);
	}

	private void fillProject(String name, String description)
	{
		_projectNameField.setText(name);
		_projectDescriptionField.setText(description);
	}

	void reloadProject()
	{
		fillProject(_controller.getProjectName(), _controller.getProjectDescription());
	}
}

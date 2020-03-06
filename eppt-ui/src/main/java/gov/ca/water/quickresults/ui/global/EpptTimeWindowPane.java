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

import com.sun.javafx.scene.control.skin.SpinnerSkin;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.project.EpptConfigurationController;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.control.TitledPane;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
class EpptTimeWindowPane extends TitledPane
{
	private final EpptConfigurationController _controller;
	private final ComboBox<WaterYearDefinition> _waterYearDefinitionComboBox;
	private final Spinner<Integer> _startYearSpinner;
	private final Spinner<Integer> _endYearSpinner;

	EpptTimeWindowPane(EpptConfigurationController controller)
	{
		ObservableList<WaterYearDefinition> waterYearDefinitions = FXCollections.observableList(
				WaterYearDefinitionSvc.getWaterYearDefinitionSvc().getDefinitions());
		_controller = controller;
		_waterYearDefinitionComboBox = new ComboBox<>(waterYearDefinitions);
		_startYearSpinner = new Spinner<>(0, Integer.MAX_VALUE, _controller.getStartYear());
		_endYearSpinner = new Spinner<>(0, Integer.MAX_VALUE, _controller.getEndYear());
		initComponents();
		initListeners();
	}

	private void initListeners()
	{
		_waterYearDefinitionComboBox.getSelectionModel().selectedItemProperty().addListener((e, o, n) -> _controller.setWaterYearDefinition(n));
		_startYearSpinner.valueProperty().addListener((e, o, n) ->
		{
			_controller.setStartYear(n);
			SpinnerValueFactory.IntegerSpinnerValueFactory valueFactory = (SpinnerValueFactory.IntegerSpinnerValueFactory)_endYearSpinner.getValueFactory();
			valueFactory.setMin(n);
		});
		_endYearSpinner.valueProperty().addListener((e, o, n) ->
		{
			_controller.setEndYear(n);
			SpinnerValueFactory.IntegerSpinnerValueFactory valueFactory = (SpinnerValueFactory.IntegerSpinnerValueFactory)_startYearSpinner.getValueFactory();
			valueFactory.setMax(n);
		});
		_startYearSpinner.addEventFilter(KeyEvent.KEY_PRESSED, new MyKeyAdapter(_startYearSpinner));
		_endYearSpinner.addEventFilter(KeyEvent.KEY_PRESSED, new MyKeyAdapter(_endYearSpinner));
	}

	private void initComponents()
	{
		setGraphicTextGap(0);
		BorderPane borderPane = new BorderPane();
		Label statisticLabel = new Label("Time Window");
		borderPane.setLeft(statisticLabel);
		BorderPane.setMargin(statisticLabel, new Insets(5));
		setGraphic(borderPane);
		GridPane timeWindowControlsGrid = new GridPane();
		timeWindowControlsGrid.setHgap(5);
		timeWindowControlsGrid.setVgap(5);
		timeWindowControlsGrid.add(new Label("Start Year: "), 0, 1);
		timeWindowControlsGrid.add(new Label("End Year: "), 0, 2);
		_startYearSpinner.setPrefWidth(70);
		_startYearSpinner.setEditable(true);
		timeWindowControlsGrid.add(_startYearSpinner, 1, 1);
		_endYearSpinner.setPrefWidth(70);
		_endYearSpinner.setEditable(true);
		timeWindowControlsGrid.add(_endYearSpinner, 1, 2);
		_waterYearDefinitionComboBox.getSelectionModel().select(0);
		_waterYearDefinitionComboBox.setMaxWidth(Double.MAX_VALUE);
		timeWindowControlsGrid.add(new Label("Water Year Definition: "), 0, 3);
		timeWindowControlsGrid.add(_waterYearDefinitionComboBox, 1, 3);
		setContent(timeWindowControlsGrid);
	}

	void reloadProject()
	{
		_startYearSpinner.getValueFactory().setValue(_controller.getStartYear());
		_endYearSpinner.getValueFactory().setValue(_controller.getEndYear());
	}

	private static final class MyKeyAdapter implements EventHandler<KeyEvent>
	{
		private final Spinner<Integer> _component;

		private MyKeyAdapter(Spinner<Integer> component)
		{
			_component = component;
		}

		@Override
		public void handle(KeyEvent event)
		{
			KeyCode key = event.getCode();
			if(key == KeyCode.ENTER)
			{
				_component.fireEvent(new KeyEvent(event.getEventType(), event.getCharacter(), event.getText(), KeyCode.TAB, event.isShiftDown(),
						event.isControlDown(), event.isAltDown(), event.isMetaDown()));
			}
		}
	}
}


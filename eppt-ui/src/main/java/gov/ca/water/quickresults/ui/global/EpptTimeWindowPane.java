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

import java.time.format.DateTimeFormatter;
import java.util.Collections;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.WaterYearDefinitionSvc;
import gov.ca.water.calgui.project.EpptConfigurationController;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.control.TitledPane;
import javafx.scene.control.Tooltip;
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
	private final Label _timeWindowRangeLabel;
	private final Button _rangeButton;
	private final Tooltip _periodRangePaneTooltip = new Tooltip();

	EpptTimeWindowPane(EpptConfigurationController controller)
	{
		ObservableList<WaterYearDefinition> waterYearDefinitions = FXCollections.observableList(WaterYearDefinitionSvc.getWaterYearDefinitionSvc().getDefinitions());
		_controller = controller;
		_waterYearDefinitionComboBox = new ComboBox<>(waterYearDefinitions);
		_startYearSpinner = new Spinner<>(0, Integer.MAX_VALUE, _controller.getWaterYearDefinition().getStartDefaultYear().orElse(1921));
		_endYearSpinner = new Spinner<>(0, Integer.MAX_VALUE, _controller.getWaterYearDefinition().getEndDefaultYear().orElse(2003));
		_timeWindowRangeLabel = new Label();
		_rangeButton = new Button("...");
		initComponents();
		initListeners();
		updateTimeWindowRangeLabel();
	}

	private void updateTimeWindowRangeLabel()
	{
		WaterYearPeriod waterYearPeriod = new WaterYearPeriod("");
		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(_startYearSpinner.getValue(), waterYearPeriod),
				new WaterYearType(_endYearSpinner.getValue(), waterYearPeriod));
		_timeWindowRangeLabel.setText("[" + waterYearPeriodRange.toString(_waterYearDefinitionComboBox.getValue(), DateTimeFormatter.ofPattern("MMM yyyy")) + "]");
		EpptPeriodRangePane epptPeriodRangePane = new EpptPeriodRangePane(_controller.getWaterYearDefinition());
		_periodRangePaneTooltip.setGraphic(epptPeriodRangePane);

		WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter("", "", Collections.singletonList(waterYearPeriodRange),
				_waterYearDefinitionComboBox.getValue());
		Platform.runLater(() -> epptPeriodRangePane.fill(waterYearPeriodRangesFilter));
	}

	private void initListeners()
	{
		_waterYearDefinitionComboBox.getSelectionModel().selectedItemProperty().addListener((e, o, n) ->
		{
			_controller.setWaterYearDefinition(n);
			_controller.setModified();
			n.getStartDefaultYear().ifPresent(y -> _startYearSpinner.getValueFactory().setValue(y));
			n.getEndDefaultYear().ifPresent(y -> _endYearSpinner.getValueFactory().setValue(y));
			updateTimeWindowRangeLabel();
		});
		_startYearSpinner.valueProperty().addListener(startYearChanged());
		_endYearSpinner.valueProperty().addListener(endYearChanged());
		_startYearSpinner.addEventFilter(KeyEvent.KEY_PRESSED, new MyKeyAdapter(_startYearSpinner));
		_endYearSpinner.addEventFilter(KeyEvent.KEY_PRESSED, new MyKeyAdapter(_endYearSpinner));
		_startYearSpinner.focusedProperty().addListener((obs, o, n) -> _startYearSpinner.increment(0));
		_endYearSpinner.focusedProperty().addListener((obs, o, n) -> _endYearSpinner.increment(0));
		_rangeButton.setOnAction(e ->
		{

		});
	}

	private ChangeListener<Integer> endYearChanged()
	{
		return (e, o, n) ->
		{
			_controller.setEndYear(n);
			SpinnerValueFactory.IntegerSpinnerValueFactory valueFactory = (SpinnerValueFactory.IntegerSpinnerValueFactory) _startYearSpinner.getValueFactory();
			valueFactory.setMax(n);
			_controller.setModified();
			updateTimeWindowRangeLabel();
		};
	}

	private ChangeListener<Integer> startYearChanged()
	{
		return (e, o, n) ->
		{
			_controller.setStartYear(n);
			SpinnerValueFactory.IntegerSpinnerValueFactory valueFactory = (SpinnerValueFactory.IntegerSpinnerValueFactory) _endYearSpinner.getValueFactory();
			valueFactory.setMin(n);
			_controller.setModified();
			updateTimeWindowRangeLabel();
		};
	}

	private void initComponents()
	{
		setGraphicTextGap(0);
		BorderPane borderPane = new BorderPane();
		Label statisticLabel = new Label("Time Window");
		borderPane.setLeft(statisticLabel);
		BorderPane.setMargin(statisticLabel, new Insets(2));
		setGraphic(borderPane);
		_startYearSpinner.setEditable(true);
		_endYearSpinner.setEditable(true);
		_waterYearDefinitionComboBox.getSelectionModel().select(0);
		_startYearSpinner.setPrefWidth(70);
		_endYearSpinner.setPrefWidth(70);
		Label waterDefinitionLabel = new Label("Water Year Definition: ");
		GridPane gridPane = new GridPane();
		gridPane.add(waterDefinitionLabel, 0, 0);
		gridPane.add(_waterYearDefinitionComboBox, 1, 0);
		gridPane.add(new Label("Start Water Year: "), 0, 1);
		gridPane.add(_startYearSpinner, 1, 1);
		gridPane.add(new Label("End Water Year: "), 0, 2);
		gridPane.add(_endYearSpinner, 1, 2);
		gridPane.add(new Label("Calendar Period:"), 0, 3);
		gridPane.add(_timeWindowRangeLabel, 1, 3);
		gridPane.setHgap(5.0);
		gridPane.setVgap(5.0);
		setContent(gridPane);
		_timeWindowRangeLabel.setTooltip(_periodRangePaneTooltip);
		_periodRangePaneTooltip.setStyle("-fx-border-width: 1px;-fx-border-insets: 1px;-fx-background-color: white;-fx-font-size:10px");
	}

	void reloadProject()
	{
		_waterYearDefinitionComboBox.setValue(_controller.getWaterYearDefinition());
		_startYearSpinner.getValueFactory().setValue(_controller.getStartYear());
		_endYearSpinner.getValueFactory().setValue(_controller.getEndYear());
	}

	public void commitEdits()
	{
		_startYearSpinner.increment(0);
		_endYearSpinner.increment(0);
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
				_component.fireEvent(
						new KeyEvent(event.getEventType(), event.getCharacter(), event.getText(), KeyCode.TAB, event.isShiftDown(), event.isControlDown(), event.isAltDown(),
								event.isMetaDown()));
			}
			else if(key == KeyCode.TAB)
			{
				updateValueFromText();
			}
		}

		private void updateValueFromText()
		{
			String text = _component.getEditor().getText();
			SpinnerValueFactory<Integer> valueFactory = _component.getValueFactory();
			Integer integer = valueFactory.getConverter().fromString(text);
			valueFactory.setValue(integer);
		}
	}
}


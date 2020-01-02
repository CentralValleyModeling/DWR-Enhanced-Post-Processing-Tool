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

package gov.ca.water.trendreporting;

import java.util.List;

import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.TilePane;
import javafx.scene.layout.VBox;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-02-2020
 */
class TrendReportPeriodPane extends TitledPane
{
	private final ListView<EpptReportingMonths.MonthPeriod> _seasonalPeriodListView = new ListView<>();
	private final AutoCompleteTextField<EpptReportingMonths.MonthPeriod> _textField = new AutoCompleteTextField<>();
	private ObservableList<EpptReportingMonths.MonthPeriod> _backingStats;
	private FilteredList<EpptReportingMonths.MonthPeriod> _filteredPeriods;

	TrendReportPeriodPane()
	{
		initComponents();
		initListeners();
	}

	private void initComponents()
	{
		_seasonalPeriodListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		List<EpptReportingMonths.MonthPeriod> allMonthPeriods = EpptReportingMonths.getAllMonthPeriods();
		_textField.getEntries().addAll(allMonthPeriods);
		_backingStats = FXCollections.observableList(allMonthPeriods);
		_filteredPeriods = new FilteredList<>(_backingStats);
		_seasonalPeriodListView.setItems(_filteredPeriods);
		_seasonalPeriodListView.getSelectionModel().select(0);
		VBox vBox = new VBox(_seasonalPeriodListView);
		vBox.setPrefHeight(200);
		TilePane innerPane = new TilePane(Orientation.VERTICAL, 5, 5, vBox);
		setContent(innerPane);
		setGraphicTextGap(0);
		Label seasonalLabel = new Label("Seasonal Period");
		BorderPane borderPane = new BorderPane();
		borderPane.setLeft(seasonalLabel);
		_textField.setPrefWidth(140);
		_textField.setPromptText("October");
		borderPane.setCenter(_textField);
		BorderPane.setMargin(seasonalLabel, new Insets(5));
		setGraphic(borderPane);
		setPrefWidth(200);
		setPrefHeight(200);
	}

	void addListener(ChangeListener<? super EpptReportingMonths.MonthPeriod> inputsChanged)
	{
		_seasonalPeriodListView.getSelectionModel().selectedItemProperty().addListener(inputsChanged);
	}

	ObservableList<EpptReportingMonths.MonthPeriod> getSelectedItems()
	{
		return _seasonalPeriodListView.getSelectionModel().getSelectedItems();
	}

	private void initListeners()
	{
		_textField.textProperty().addListener((observable, oldValue, newValue) -> textChanged());
		_textField.setEntryPicked(_seasonalPeriodListView.getSelectionModel()::select);
	}

	private void textChanged()
	{
		EpptReportingMonths.MonthPeriod selectedItem = _seasonalPeriodListView.getSelectionModel().getSelectedItem();
		String text = _textField.getText();
		if(text.trim().isEmpty())
		{
			_filteredPeriods.setPredicate(s->true);
		}
		else
		{
			_filteredPeriods.setPredicate(s->s.toString().toLowerCase().contains(text.toLowerCase()));
		}
		if(selectedItem != null && _filteredPeriods.contains(selectedItem))
		{
			_seasonalPeriodListView.getSelectionModel().select(selectedItem);
		}
	}
}

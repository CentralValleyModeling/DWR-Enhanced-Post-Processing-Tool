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

package gov.ca.water.quickresults.ui.trendreporting;

import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;

import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.ScriptedEpptStatistics;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.MultipleSelectionModel;
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
public class TrendReportStatisticsPane extends TitledPane
{
	private static final Logger LOGGER = Logger.getLogger(TrendReportStatisticsPane.class.getName());

	private final ListView<EpptStatistic> _statisticsListView = new ListView<>();
	private FilteredList<EpptStatistic> _filteredStats;
	private ObservableList<EpptStatistic> _backingStats;
	private final AutoCompleteTextField<EpptStatistic> _textField = new AutoCompleteTextField<>();

	public TrendReportStatisticsPane()
	{
		initComponents();
		initListeners();
		loadStats();
	}

	private void initListeners()
	{
		_textField.textProperty().addListener((observable, oldValue, newValue) -> textChanged());
		MultipleSelectionModel<EpptStatistic> selectionModel = _statisticsListView.getSelectionModel();
		_textField.setEntryPicked(selectionModel::select);
	}

	private void textChanged()
	{
		String text = _textField.getText();
		EpptStatistic selectedItem = _statisticsListView.getSelectionModel().getSelectedItem();
		if(text.trim().isEmpty())
		{
			_filteredStats.setPredicate(s->true);
		}
		else
		{
			_filteredStats.setPredicate(s->s.getName().toLowerCase().contains(text.toLowerCase()));
		}
		if(selectedItem != null && _filteredStats.contains(selectedItem))
		{
			_statisticsListView.getSelectionModel().select(selectedItem);
		}
	}

	private void loadStats()
	{
		CompletableFuture.supplyAsync(ScriptedEpptStatistics::getTrendStatistics)
						 .whenComplete((o, t) ->
						 {
							 if(t != null)
							 {
								 LOGGER.log(Level.SEVERE, "Error loading Trend Report Statistics", t);
							 }
							 else
							 {
								 Platform.runLater(() ->
								 {
									 _textField.getEntries().addAll(o);
									 _backingStats = FXCollections.observableList(o);
									 _filteredStats = new FilteredList<>(_backingStats);
									 _statisticsListView.setItems(_filteredStats);
									 _statisticsListView.getSelectionModel().select(0);
								 });
							 }
						 });
	}

	private void initComponents()
	{
		_statisticsListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		VBox vBox = new VBox(_statisticsListView);
		vBox.setPrefWidth(250);
		vBox.setPrefHeight(200);
		TilePane innerPane = new TilePane(Orientation.VERTICAL, 5, 5, vBox);
		setContent(innerPane);
		setGraphicTextGap(0);
		BorderPane borderPane = new BorderPane();
		Label statisticLabel = new Label("Statistic");
		borderPane.setLeft(statisticLabel);
		_textField.setPrefWidth(185);
		_textField.setPromptText("Averages");
		borderPane.setCenter(_textField);
		BorderPane.setMargin(statisticLabel, new Insets(5));
		setGraphic(borderPane);
		setPrefWidth(250);
		setPrefHeight(200);
	}

	public ObservableList<EpptStatistic> getSelectedItems()
	{
		return _statisticsListView.getSelectionModel().getSelectedItems();
	}

	public void addListener(ChangeListener<? super EpptStatistic> inputsChanged)
	{
		_statisticsListView.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
		{
			inputsChanged.changed(observable, oldValue, newValue);
			if(newValue != null)
			{
				_textField.setPromptText(newValue.toString());
			}
		});
	}
}

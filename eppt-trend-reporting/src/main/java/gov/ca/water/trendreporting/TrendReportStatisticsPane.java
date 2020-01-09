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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

import gov.ca.water.calgui.constant.Constant;
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

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-02-2020
 */
class TrendReportStatisticsPane extends TitledPane
{
	private static final Logger LOGGER = Logger.getLogger(TrendReportStatisticsPane.class.getName());

	private final ListView<TrendStatistics> _statisticsListView = new ListView<>();
	private FilteredList<TrendStatistics> _filteredStats;
	private ObservableList<TrendStatistics> _backingStats;
	private final AutoCompleteTextField<TrendStatistics> _textField = new AutoCompleteTextField<>();

	TrendReportStatisticsPane()
	{
		initComponents();
		initListeners();
		loadStats();
	}

	private void initListeners()
	{
		_textField.textProperty().addListener((observable, oldValue, newValue) -> textChanged());
		MultipleSelectionModel<TrendStatistics> selectionModel = _statisticsListView.getSelectionModel();
		_textField.setEntryPicked(obj -> selectionModel.select(obj));
	}

	private void textChanged()
	{
		String text = _textField.getText();
		TrendStatistics selectedItem = _statisticsListView.getSelectionModel().getSelectedItem();
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
		CompletableFuture.supplyAsync(this::getTrendStatistics)
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

	private List<TrendStatistics> getTrendStatistics()
	{
		List<TrendStatistics> retval = new ArrayList<>();
		Path jython = Paths.get(Constant.TREND_REPORTING_DIR).resolve("jython");
		try(Stream<Path> stream = Files.walk(jython, 5))
		{
			retval = stream.filter(p -> p.toFile().isFile()).filter(p -> p.toString().endsWith("py"))
						   .map(TrendStatistics::new)
						   .collect(toList());
		}
		catch(IOException e)
		{
			LOGGER.log(Level.SEVERE, "Unable to load Statistics for Trend Reporting dashboard", e);
		}
		return retval;
	}

	ObservableList<TrendStatistics> getSelectedItems()
	{
		return _statisticsListView.getSelectionModel().getSelectedItems();
	}

	void addListener(ChangeListener<? super TrendStatistics> inputsChanged)
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

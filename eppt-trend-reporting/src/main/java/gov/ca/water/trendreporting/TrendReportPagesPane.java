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
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.xml.parsers.ParserConfigurationException;

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.quickresults.ui.global.AutoCompleteTextField;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TitledPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.HBox;
import org.xml.sax.SAXException;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-01-2020
 */
class TrendReportPagesPane extends TitledPane
{
	private static final Logger LOGGER = Logger.getLogger(TrendReportPagesPane.class.getName());
	private final Runnable _updateTrigger;
	private final ListView<TrendReportTabConfig> _trendReportTabConfigListView = new ListView<>();
	private final AutoCompleteTextField<TrendReportTabConfig> _textField = new AutoCompleteTextField<>();
	private final Button _leftButton = new Button("<");
	private final Button _rightButton = new Button(">");

	TrendReportPagesPane(Runnable updateTrigger)
	{
		_updateTrigger = updateTrigger;
		initComponents();
		initListeners();
	}

	private void initComponents()
	{
		ObservableList<TrendReportTabConfig> backingParameters = FXCollections.observableArrayList();
		FilteredList<TrendReportTabConfig> filteredParameters = new FilteredList<>(backingParameters, s -> true);
		_trendReportTabConfigListView.setItems(filteredParameters);
		try(Stream<Path> paths = Files.walk(Paths.get(Constant.TREND_REPORTING_DIR), 1))
		{
			backingParameters.addAll(paths.filter(path -> path.toString().endsWith(".htm") || path.toString().endsWith(".html"))
										  .map(this::buildTrendReportTabConfig)
										  .filter(Objects::nonNull)
										  .collect(toList()));
			_trendReportTabConfigListView.getSelectionModel().select(0);
		}
		catch(IOException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to build javascript panels", ex);
		}
		BorderPane borderPane = new BorderPane();
		HBox hBox = new HBox();
		_leftButton.setPrefHeight(25);
		_rightButton.setPrefHeight(25);
		_textField.setPromptText(backingParameters.get(0).toString());
		_textField.getEntries().addAll(backingParameters);
		borderPane.setCenter(_trendReportTabConfigListView);

		Label pagesLabel = new Label("Reports");
		FlowPane.setMargin(pagesLabel, new Insets(5));
		hBox.getChildren().addAll(_leftButton, _rightButton);
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL, 5, 5);
		flowPane.getChildren().addAll(pagesLabel, _textField, hBox);

		setGraphicTextGap(0);
		setGraphic(flowPane);
		setContent(borderPane);
		setMaxHeight(Double.MAX_VALUE);
		borderPane.setPrefWidth(120);
		borderPane.setPrefHeight(200);
		setMaxHeight(Double.MAX_VALUE);
		setPrefHeight(200);
	}

	private TrendReportTabConfig buildTrendReportTabConfig(Path path)
	{
		try
		{
			return new TrendReportTabConfig(path);
		}
		catch(IOException | RuntimeException | ParserConfigurationException | SAXException ex)
		{
			LOGGER.log(Level.SEVERE, "Unable to extract toggle button text from: " + path, ex);
			return null;
		}
	}

	TrendReportTabConfig getTrendReportTabConfig()
	{
		return _trendReportTabConfigListView.getSelectionModel().getSelectedItem();
	}

	private void initListeners()
	{
		_trendReportTabConfigListView.getSelectionModel().selectedItemProperty().addListener(e ->
		{
			_textField.setPromptText(Objects.toString(_trendReportTabConfigListView.getSelectionModel().getSelectedItem()));
			_updateTrigger.run();
		});
		_rightButton.setOnAction(e ->
		{
			int selectedIndex = _trendReportTabConfigListView.getSelectionModel().getSelectedIndex() + 1;
			if(selectedIndex >= _trendReportTabConfigListView.getItems().size())
			{
				selectedIndex = 0;
			}
			_trendReportTabConfigListView.getSelectionModel().select(selectedIndex);
		});
		_leftButton.setOnAction(e ->
		{
			int selectedIndex = _trendReportTabConfigListView.getSelectionModel().getSelectedIndex() - 1;
			if(selectedIndex < 0)
			{
				selectedIndex = _trendReportTabConfigListView.getItems().size() - 1;
			}
			_trendReportTabConfigListView.getSelectionModel().select(selectedIndex);
		});
		_textField.setEntryPicked(obj ->
		{
			_trendReportTabConfigListView.getSelectionModel().select(obj);
			_trendReportTabConfigListView.getSelectionModel().getSelectedItem();
		});
	}
}

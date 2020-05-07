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

import java.util.List;

import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import gov.ca.water.calgui.busservice.impl.ScriptedEpptStatistics;
import gov.ca.water.calgui.project.EpptConfigurationController;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TitledPane;
import javafx.scene.control.cell.CheckBoxListCell;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;
import javafx.scene.text.Font;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-02-2020
 */
class EpptStatisticsPane extends TitledPane
{
	private final ListView<StatItem> _statisticsListView = new ListView<>();
	private final EpptConfigurationController _controller;
	private boolean _modifiedListenerEnabled = false;
	private final CheckBox _tafCheckbox = new CheckBox("Convert CFS to TAF");

	EpptStatisticsPane(EpptConfigurationController controller)
	{
		_controller = controller;
		initComponents();
		loadStats();
		initListeners();
	}

	private void initListeners()
	{
		_tafCheckbox.selectedProperty().addListener((e, o, n) ->
		{
			_controller.setTaf(n);
			if(_modifiedListenerEnabled)
			{
				_controller.setModified();
			}
		});
	}

	private void loadStats()
	{
		List<EpptStatistic> statistics = ScriptedEpptStatistics.getTrendStatistics();
		_statisticsListView.setItems(FXCollections.observableList(statistics.stream().map(StatItem::new).collect(toList())));
	}

	private void initComponents()
	{
		BorderPane content = new BorderPane();
		_statisticsListView.setCellFactory(listView -> new MyCheckboxTreeItem());
		_statisticsListView.setFixedCellSize(20);
		_statisticsListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL);
		BorderPane.setMargin(flowPane, new Insets(5, 5, 5, 7));
		flowPane.getChildren().add(_tafCheckbox);
		flowPane.setAlignment(Pos.CENTER_LEFT);
		content.setTop(flowPane);
		content.setCenter(_statisticsListView);
		setContent(content);
		setGraphicTextGap(0);
		BorderPane borderPane = new BorderPane();
		Label statisticLabel = new Label("Statistic");
		borderPane.setLeft(statisticLabel);
		BorderPane.setMargin(statisticLabel, new Insets(2));
		setGraphic(borderPane);
		setPrefHeight(280);
	}

	private List<EpptStatistic> getSelectedStatistic()
	{
		return _statisticsListView.getItems()
								  .stream()
								  .filter(item -> item._selected.get())
								  .map(item -> item._epptStatistic)
								  .collect(toList());
	}

	void reloadProject()
	{
		try
		{
			_modifiedListenerEnabled = false;
			_tafCheckbox.setSelected(_controller.isTaf());
			List<EpptStatistic> selectedStatistics = _controller.getSelectedStatistics();
			_statisticsListView.getItems().forEach(i -> i._selected.set(selectedStatistics.stream()
																						  .map(EpptStatistic::getName)
																						  .anyMatch(s -> s.equals(i._epptStatistic.getName()))));
		}
		finally
		{
			_modifiedListenerEnabled = true;
		}
	}

	private final class MyCheckboxTreeItem extends CheckBoxListCell<StatItem>
	{
		private MyCheckboxTreeItem()
		{
			super(param -> param._selected);
			super.setPrefHeight(20);
			super.setFont(Font.font(11));
		}

		@Override
		public void updateItem(StatItem item, boolean empty)
		{
			super.updateItem(item, empty);
			if(item != null && item.toString().trim().isEmpty())
			{
				setGraphic(null);
			}
			CheckBox graphic = (CheckBox) getGraphic();
			if(graphic != null)
			{
				graphic.selectedProperty().addListener((e, o, n) ->
				{
					if(_modifiedListenerEnabled)
					{
						_controller.setStatistics(getSelectedStatistic());
						_controller.setModified();
					}
				});
			}
		}
	}

	private static final class StatItem
	{
		private final EpptStatistic _epptStatistic;
		private SimpleBooleanProperty _selected = new SimpleBooleanProperty(false);

		private StatItem(EpptStatistic statistic)
		{
			_epptStatistic = statistic;
		}

		@Override
		public String toString()
		{
			return _epptStatistic.toString();
		}
	}

}

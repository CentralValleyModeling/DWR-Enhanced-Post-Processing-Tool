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

import java.awt.Frame;
import java.lang.reflect.InvocationTargetException;
import java.time.Month;
import java.time.format.TextStyle;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import gov.ca.water.calgui.busservice.impl.EpptReportingMonths;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.project.EpptConfigurationController;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TitledPane;
import javafx.scene.control.cell.CheckBoxListCell;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-02-2020
 */
class EpptMonthlyPeriodPane extends TitledPane
{
	private static final Logger LOGGER = Logger.getLogger(EpptMonthlyPeriodPane.class.getName());
	private final MonthPeriod _entirePeriod = new MonthPeriod(null, null, null)
	{
		@Override
		public Month getStart()
		{
			return _controller.getWaterYearDefinition().getStartMonth();
		}

		@Override
		public Month getEnd()
		{
			return _controller.getWaterYearDefinition().getEndMonth();
		}

		@Override
		public String toString()
		{
			return "Entire Period (" + getStart().getDisplayName(TextStyle.SHORT, Locale.getDefault()) + " - " +
					getEnd().getDisplayName(TextStyle.SHORT, Locale.getDefault()) + ")";
		}
	};
	private final ListView<PeriodItem> _seasonalPeriodListView = new ListView<>();
	private final EpptConfigurationController _controller;
	private final Button _editButton = new Button("Edit");

	EpptMonthlyPeriodPane(EpptConfigurationController controller)
	{
		_controller = controller;
		initComponents();
		addListeners();
		_seasonalPeriodListView.getItems().get(0)._selected.set(true);
	}

	private void addListeners()
	{
		_controller.waterYearDefinitionProperty().addListener((e, o, n) -> _seasonalPeriodListView.refresh());
		_seasonalPeriodListView.getSelectionModel().selectedItemProperty().addListener(
				(e, o, n) -> _editButton.setDisable(n == null || !n._editable));
	}

	private void initComponents()
	{
		_seasonalPeriodListView.setCellFactory(listView -> new MyCheckboxTreeItem());
		_seasonalPeriodListView.setStyle("-fx-selection-bar:-fx-focus-color ;-fx-selection-bar-non-focused: -fx-focus-color ;");
		List<MonthPeriod> allMonthPeriods = EpptReportingMonths.getInstance().getAllMonthPeriods();
		_seasonalPeriodListView.setItems(FXCollections.observableList(allMonthPeriods.stream().map(PeriodItem::new).collect(toList())));
		_seasonalPeriodListView.getItems().add(0, new PeriodItem(new MonthPeriod(null, null, null)));
		PeriodItem entirePeriodItem = new PeriodItem(_entirePeriod);
		_seasonalPeriodListView.getItems().add(0, entirePeriodItem);
		BorderPane innerPane = new BorderPane();
		innerPane.setCenter(_seasonalPeriodListView);
		setContent(innerPane);
		setGraphicTextGap(0);
		Label seasonalLabel = new Label("Seasonal Period");
		BorderPane graphicPane = new BorderPane();
		graphicPane.setLeft(seasonalLabel);
		Button addButton = new Button("Add");
		addButton.setOnAction(e -> addMonthlyPeriod());
		_editButton.setDisable(true);
		_editButton.setOnAction(e -> editMonthlyPeriod());
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL, addButton, _editButton);
		flowPane.setAlignment(Pos.CENTER_RIGHT);
		flowPane.setPrefWidth(140);
		graphicPane.setRight(flowPane);
		BorderPane.setMargin(seasonalLabel, new Insets(2));
		setGraphic(graphicPane);
	}

	private void editMonthlyPeriod()
	{
		int selectedIndex = _seasonalPeriodListView.getSelectionModel().getSelectedIndex();
		PeriodItem selectedItem = _seasonalPeriodListView.getSelectionModel().getSelectedItem();
		if(selectedItem != null && selectedItem._editable)
		{
			AddMonthlyFilterDialog addMonthlyFilterDialog = new AddMonthlyFilterDialog(Frame.getFrames()[0], selectedItem._monthPeriod);
			try
			{
				SwingUtilities.invokeAndWait(() -> addMonthlyFilterDialog.setVisible(true));
				if(!addMonthlyFilterDialog.isCanceled())
				{
					String name = addMonthlyFilterDialog.getPeriodName();
					Month startMonth = addMonthlyFilterDialog.getStartMonth();
					Month endMonth = addMonthlyFilterDialog.getEndMonth();
					_seasonalPeriodListView.getItems().remove(selectedIndex);
					PeriodItem periodItem = new PeriodItem(new MonthPeriod(name, startMonth, endMonth), true);
					_seasonalPeriodListView.getItems().add(selectedIndex, periodItem);
					_seasonalPeriodListView.getSelectionModel().select(periodItem);
				}
			}
			catch(InterruptedException e)
			{
				Thread.currentThread().interrupt();
				LOGGER.log(Level.FINE, "Thread interrupted while adding monthly period", e);
			}
			catch(InvocationTargetException e)
			{
				LOGGER.log(Level.SEVERE, "Error adding monthly period", e);
			}
		}
	}

	private List<MonthPeriod> getSelectedMonthlyPeriods()
	{
		return _seasonalPeriodListView.getItems()
									  .stream()
									  .filter(item -> item._selected.get())
									  .map(item -> item._monthPeriod)
									  .collect(toList());
	}

	private void addMonthlyPeriod()
	{
		AddMonthlyFilterDialog addMonthlyFilterDialog = new AddMonthlyFilterDialog(Frame.getFrames()[0]);
		try
		{
			SwingUtilities.invokeAndWait(() -> addMonthlyFilterDialog.setVisible(true));
			if(!addMonthlyFilterDialog.isCanceled())
			{
				String name = addMonthlyFilterDialog.getPeriodName();
				Month startMonth = addMonthlyFilterDialog.getStartMonth();
				Month endMonth = addMonthlyFilterDialog.getEndMonth();
				_seasonalPeriodListView.getItems().add(new PeriodItem(new MonthPeriod(name, startMonth, endMonth), true));
			}
		}
		catch(InterruptedException e)
		{
			Thread.currentThread().interrupt();
			LOGGER.log(Level.FINE, "Thread interrupted while adding monthly period", e);
		}
		catch(InvocationTargetException e)
		{
			LOGGER.log(Level.SEVERE, "Error adding monthly period", e);
		}
	}

	private final class MyCheckboxTreeItem extends CheckBoxListCell<PeriodItem>
	{
		private MyCheckboxTreeItem()
		{
			super(param -> param._selected);
		}

		@Override
		public void updateItem(PeriodItem item, boolean empty)
		{
			super.updateItem(item, empty);
			if(item != null && item.toString().trim().isEmpty())
			{
				setGraphic(null);
			}
			CheckBox graphic = (CheckBox) getGraphic();
			if(graphic != null)
			{
				graphic.selectedProperty().addListener((e, o, n) -> _controller.setMonthlyPeriods(getSelectedMonthlyPeriods()));
			}
		}
	}

	private static final class PeriodItem
	{
		private final MonthPeriod _monthPeriod;
		private final boolean _editable;
		private SimpleBooleanProperty _selected = new SimpleBooleanProperty(false);

		private PeriodItem(MonthPeriod period)
		{
			_editable = false;
			_monthPeriod = period;
		}

		private PeriodItem(MonthPeriod period, boolean editable)
		{
			_monthPeriod = period;
			_editable = editable;
		}

		@Override
		public String toString()
		{
			return _monthPeriod.toString();
		}
	}
}

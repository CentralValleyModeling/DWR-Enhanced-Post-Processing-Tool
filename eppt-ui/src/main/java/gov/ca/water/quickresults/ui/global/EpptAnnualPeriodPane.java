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
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.WaterYearIndexAliasReader;
import gov.ca.water.calgui.busservice.impl.WaterYearPeriodReader;
import gov.ca.water.calgui.project.EpptConfigurationController;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBoxTreeItem;
import javafx.scene.control.Label;
import javafx.scene.control.TitledPane;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.control.cell.CheckBoxTreeCell;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.FlowPane;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-20-2019
 */
class EpptAnnualPeriodPane extends TitledPane
{
	private static final Logger LOGGER = Logger.getLogger(EpptAnnualPeriodPane.class.getName());
	private final TreeView<WaterYearPeriodDefinitionsRow> _treeView = new TreeView<>();
	private final EpptConfigurationController _controller;
	private final Button _editButton = new Button("Edit");

	EpptAnnualPeriodPane(EpptConfigurationController controller)
	{
		_controller = controller;
		initComponents();
		addListeners();
		((MyCheckBoxTreeItem) _treeView.getRoot().getChildren().get(0)).setSelected(true);
	}

	private void addListeners()
	{
		_controller.waterYearDefinitionProperty().addListener((e, o, n) -> _treeView.refresh());
		_controller.startYearProperty().addListener((e, o, n) -> _treeView.refresh());
		_controller.endYearProperty().addListener((e, o, n) -> _treeView.refresh());
		_treeView.getSelectionModel().selectedItemProperty().addListener(
				(e, o, n) -> _editButton.setDisable(n == null || n.getValue() == null || !n.getValue().isEditable()));
	}

	private void initComponents()
	{
		initTree();
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_treeView);
		setContent(borderPane);
		setGraphicTextGap(0);
		Label seasonalLabel = new Label("Annual Period");
		BorderPane graphicPane = new BorderPane();
		graphicPane.setLeft(seasonalLabel);
		Button addButton = new Button("Add");
		addButton.setOnAction(e -> addAnnualPeriod());
		_editButton.setDisable(true);
		_editButton.setOnAction(e -> editAnnualPeriod());
		FlowPane flowPane = new FlowPane(Orientation.HORIZONTAL, addButton, _editButton);
		flowPane.setAlignment(Pos.CENTER_RIGHT);
		flowPane.setPrefWidth(150);
		graphicPane.setRight(flowPane);
		BorderPane.setMargin(seasonalLabel, new Insets(5));
		setGraphic(graphicPane);
	}

	private void editAnnualPeriod()
	{
		int selectedIndex = _treeView.getSelectionModel().getSelectedIndex();
		TreeItem<WaterYearPeriodDefinitionsRow> selectedItem = _treeView.getSelectionModel().getSelectedItem();
		if(selectedItem != null && selectedItem.getValue() != null && selectedItem.getValue().isEditable())
		{
			AddAnnualFilterDialog addMonthlyFilterDialog = new AddAnnualFilterDialog(Frame.getFrames()[0], null);
			try
			{
				SwingUtilities.invokeAndWait(() -> addMonthlyFilterDialog.setVisible(true));
				if(!addMonthlyFilterDialog.isCanceled())
				{
					String name = addMonthlyFilterDialog.getPeriodName();
					int startYear = addMonthlyFilterDialog.getStartYear();
					int endYear = addMonthlyFilterDialog.getEndYear();
					//					_treeView.getItems().remove(selectedIndex);
					//					EpptMonthlyPeriodPane.PeriodItem periodItem = new EpptMonthlyPeriodPane.PeriodItem(new MonthPeriod(name, startMonth, endMonth), true);
					//					_treeView.getItems().add(selectedIndex, periodItem);
					//					_treeView.getSelectionModel().select(periodItem);
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

	private void addAnnualPeriod()
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
				//				_treeView.getItems().add(new EpptMonthlyPeriodPane.PeriodItem(new MonthPeriod(name, startMonth, endMonth), true));
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

	private void expandTree()
	{
		for(int i = 0; i < _treeView.getRoot().getChildren().size(); i++)
		{
			TreeItem<WaterYearPeriodDefinitionsRow> treeItem = _treeView.getRoot().getChildren().get(i);
			if(treeItem != null)
			{
				treeItem.setExpanded(true);
			}
		}
	}

	private void initTree()
	{
		_treeView.setCellFactory(CheckBoxTreeCell.forTreeView());
		_treeView.setShowRoot(false);
		MyCheckBoxTreeItem rootItem = new MyCheckBoxTreeItem(new WaterYearPeriodRow(new WaterYearPeriod("Root")));
		_treeView.setRoot(rootItem);
		_treeView.setCellFactory((TreeView<WaterYearPeriodDefinitionsRow> item) -> new CheckBoxTreeCell<>());
		buildWaterYearTypeRangeRows();
		expandTree();
	}

	private void buildWaterYearTypeRangeRows()
	{
		TreeItem<WaterYearPeriodDefinitionsRow> root = _treeView.getRoot();
		root.getChildren().clear();
		buildLongTermPeriod(root);
		buildWaterYearIndexes(root);
		buildContiguousPeriods(root);
	}

	private void buildWaterYearIndexes(TreeItem<WaterYearPeriodDefinitionsRow> root)
	{
		List<WaterYearIndexAliasReader.WaterYearIndexAlias> aliases = WaterYearIndexAliasReader.getInstance().getAliases();
		for(WaterYearIndexAliasReader.WaterYearIndexAlias alias : aliases)
		{
			MyCheckBoxTreeItem parent = new MyCheckBoxTreeItem(new WaterYearIndexRow(alias));
			parent.selectedProperty().addListener((e, o, n) -> parentCheckboxSelected(e, o, n, parent));
			root.getChildren().add(parent);
			List<WaterYearPeriod> waterYearPeriods = alias.getWaterYearPeriods();
			waterYearPeriods.stream()
							.map(this::buildWaterYearIndexTypeRow)
							.forEach(parent.getChildren()::add);
		}
	}

	private MyCheckBoxTreeItem buildWaterYearIndexTypeRow(WaterYearPeriod period)
	{
		return new MyCheckBoxTreeItem(new WaterYearIndexTypeRow(period));
	}

	private void buildLongTermPeriod(TreeItem<WaterYearPeriodDefinitionsRow> root)
	{
		root.getChildren().add(new MyCheckBoxTreeItem(new LongTermRow()));
	}

	private void buildContiguousPeriods(TreeItem<WaterYearPeriodDefinitionsRow> root)
	{
		List<WaterYearPeriodReader.WaterYearPeriodDefinition> waterYearPeriodDefinitions = WaterYearPeriodReader.getInstance().getWaterYearPeriodDefinitions();
		List<String> periodNames = waterYearPeriodDefinitions.stream()
															 .map(WaterYearPeriodReader.WaterYearPeriodDefinition::getName)
															 .distinct()
															 .collect(toList());
		for(String period : periodNames)
		{
			MyCheckBoxTreeItem parent = new MyCheckBoxTreeItem(new WaterYearPeriodRow(new WaterYearPeriod(period)));
			parent.selectedProperty().addListener((e, o, n) -> parentCheckboxSelected(e, o, n, parent));
			root.getChildren().add(parent);
			waterYearPeriodDefinitions.stream()
									  .filter(s -> s.getName().equalsIgnoreCase(period))
									  .map(this::buildPeriodRow)
									  .forEach(parent.getChildren()::add);
		}
	}

	private MyCheckBoxTreeItem buildPeriodRow(WaterYearPeriodReader.WaterYearPeriodDefinition range)
	{
		WaterYearPeriod waterYearPeriod = new WaterYearPeriod(range.getName());
		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod,
				new WaterYearType(range.getStartYear(), waterYearPeriod), new WaterYearType(range.getEndYear(), waterYearPeriod));
		WaterYearPeriodRangeRow waterYearPeriodRangeRow = new WaterYearPeriodRangeRow(waterYearPeriodRange);
		return new MyCheckBoxTreeItem(waterYearPeriodRangeRow);
	}

	private void parentCheckboxSelected(ObservableValue<? extends Boolean> e, Boolean o, Boolean selected, MyCheckBoxTreeItem parent)
	{
		ObservableList<TreeItem<WaterYearPeriodDefinitionsRow>> children = parent.getChildren();
		if(selected)
		{
			for(TreeItem<WaterYearPeriodDefinitionsRow> child : children)
			{
				if(child instanceof MyCheckBoxTreeItem)
				{
					MyCheckBoxTreeItem checkBoxTreeItem = (MyCheckBoxTreeItem) child;
					if(!checkBoxTreeItem.isSelected())
					{
						checkBoxTreeItem.setSelected(true);
					}
				}
			}
		}
		else
		{
			children.stream().filter(child -> child instanceof MyCheckBoxTreeItem)
					.map(child -> (MyCheckBoxTreeItem) child)
					.forEach(child -> child.setSelected(false));
		}
	}

	public List<WaterYearPeriodRangesFilter> getWaterYearPeriodRanges()
	{
		List<WaterYearPeriodRangesFilter> retval = new ArrayList<>();
		for(TreeItem<WaterYearPeriodDefinitionsRow> treeItem : _treeView.getRoot().getChildren())
		{
			if(treeItem instanceof MyCheckBoxTreeItem)
			{
				WaterYearPeriodDefinitionsRow parentValue = treeItem.getValue();
				if(parentValue instanceof WaterYearPeriodRow)
				{
					for(TreeItem<WaterYearPeriodDefinitionsRow> child : treeItem.getChildren())
					{
						if(child instanceof MyCheckBoxTreeItem && ((MyCheckBoxTreeItem) child).isSelected())
						{
							WaterYearPeriodDefinitionsRow childValue = child.getValue();
							retval.add(childValue.getWaterYearPeriodRangesFilter());
						}
					}
				}
			}
		}
		return retval;
	}

	private interface WaterYearPeriodDefinitionsRow
	{
		String toString();

		default boolean isEditable()
		{
			return false;
		}

		WaterYearPeriodRangesFilter getWaterYearPeriodRangesFilter();
	}

	private final class LongTermRow implements WaterYearPeriodDefinitionsRow
	{

		@Override
		public String toString()
		{
			DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MMM yyyy");
			WaterYearPeriod waterYearPeriod = new WaterYearPeriod("Long Term");
			WaterYearPeriodRange range = new WaterYearPeriodRange(waterYearPeriod,
					new WaterYearType(_controller.getStartYear(), waterYearPeriod), new WaterYearType(_controller.getEndYear(), waterYearPeriod));
			return "Long Term (" + range.toString(_controller.getWaterYearDefinition(), formatter) + ")";
		}

		@Override
		public WaterYearPeriodRangesFilter getWaterYearPeriodRangesFilter()
		{
			WaterYearPeriod waterYearPeriod = new WaterYearPeriod("Long Term");
			WaterYearType start = new WaterYearType(_controller.getStartYear(), waterYearPeriod);
			WaterYearType end = new WaterYearType(_controller.getEndYear(), waterYearPeriod);
			List<WaterYearPeriodRange> waterYearPeriodRanges = Collections.singletonList(new WaterYearPeriodRange(waterYearPeriod, start, end));
			return new WaterYearPeriodRangesFilter(waterYearPeriod.getPeriodName(), waterYearPeriodRanges, _controller.getWaterYearDefinition());
		}
	}

	private static final class WaterYearPeriodRow implements WaterYearPeriodDefinitionsRow
	{
		private final WaterYearPeriod _period;

		private WaterYearPeriodRow(WaterYearPeriod period)
		{
			_period = period;
		}

		@Override
		public String toString()
		{
			return _period.toString();
		}

		@Override
		public WaterYearPeriodRangesFilter getWaterYearPeriodRangesFilter()
		{
			return null;
		}
	}

	private final class WaterYearIndexRow implements WaterYearPeriodDefinitionsRow
	{

		private final WaterYearIndexAliasReader.WaterYearIndexAlias _alias;

		private WaterYearIndexRow(WaterYearIndexAliasReader.WaterYearIndexAlias alias)
		{
			_alias = alias;
		}

		@Override
		public String toString()
		{
			return _alias.getAlias();
		}

		@Override
		public WaterYearPeriodRangesFilter getWaterYearPeriodRangesFilter()
		{
			return null;
		}
	}

	private static final class WaterYearIndexTypeRow implements WaterYearPeriodDefinitionsRow
	{
		private final WaterYearPeriod _period;

		private WaterYearIndexTypeRow(WaterYearPeriod period)
		{
			_period = period;
		}

		@Override
		public String toString()
		{
			return _period.getPeriodName();
		}

		@Override
		public WaterYearPeriodRangesFilter getWaterYearPeriodRangesFilter()
		{
			return null;
		}
	}

	private final class WaterYearPeriodRangeRow implements WaterYearPeriodDefinitionsRow
	{
		private final WaterYearPeriodRange _range;

		private WaterYearPeriodRangeRow(WaterYearPeriodRange range)
		{
			_range = range;
		}

		@Override
		public String toString()
		{
			DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MMM yyyy");
			return _range.toString(_controller.getWaterYearDefinition(), formatter);
		}

		@Override
		public WaterYearPeriodRangesFilter getWaterYearPeriodRangesFilter()
		{
			return null;
		}
	}

	private final class MyCheckBoxTreeItem extends CheckBoxTreeItem<WaterYearPeriodDefinitionsRow>
	{
		private MyCheckBoxTreeItem(WaterYearPeriodDefinitionsRow row)
		{
			super(row);
			selectedProperty().addListener((e,o,n)->_controller.setWaterYearPeriodRangesFilters(getWaterYearPeriodRanges()));
		}
	}
}

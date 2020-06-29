/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 *  EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 *  under the GNU General Public License, version 2. This means it can be
 *  copied, distributed, and modified freely, but you may not restrict others
 *  in their ability to copy, distribute, and modify it. See the license below
 *  for more details.
 *
 *  GNU General Public License
 */

package gov.ca.water.quickresults.ui.global;

import java.awt.*;
import java.lang.reflect.InvocationTargetException;
import java.time.Month;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.*;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndexModel;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.WaterYearIndexReader;
import gov.ca.water.calgui.busservice.impl.WaterYearPeriodReader;
import gov.ca.water.calgui.busservice.impl.WaterYearTableReader;
import gov.ca.water.calgui.project.EpptConfigurationController;
import gov.ca.water.calgui.project.EpptScenarioRun;
import javafx.application.Platform;
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
import javafx.scene.text.Font;

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
	private boolean _modifiedListenerEnabled = false;

	EpptAnnualPeriodPane(EpptConfigurationController controller)
	{
		_controller = controller;
		initComponents();
		addListeners();
	}

	private void addListeners()
	{
		_controller.waterYearDefinitionProperty().addListener((e, o, n) -> _treeView.refresh());
		_controller.startYearProperty().addListener((e, o, n) -> _treeView.refresh());
		_controller.endYearProperty().addListener((e, o, n) -> _treeView.refresh());
		_treeView.getSelectionModel().selectedItemProperty().addListener((e, o, n) -> _editButton.setDisable(n == null || n.getValue() == null || !n.getValue().isEditable()));
		_controller.addScenarioChangedListener((b, a) -> _controller.setWaterYearPeriodRangesFilters(getWaterYearPeriodRanges()));
		_controller.modifiedProperty().addListener((a, b, c) -> _controller.setWaterYearPeriodRangesFilters(getWaterYearPeriodRanges()));
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
		flowPane.setPrefWidth(140);
		graphicPane.setRight(flowPane);
		BorderPane.setMargin(seasonalLabel, new Insets(2));
		setGraphic(graphicPane);
		setPrefWidth(140);
	}

	private void editAnnualPeriod()
	{
		TreeItem<WaterYearPeriodDefinitionsRow> selectedItem = _treeView.getSelectionModel().getSelectedItem();
		if(selectedItem != null)
		{
			WaterYearPeriodDefinitionsRow rowModel = selectedItem.getValue();
			if(rowModel != null && rowModel.isEditable() && rowModel instanceof WaterYearPeriodRangeRow)
			{
				WaterYearPeriodRangeRow rangeRowModel = (WaterYearPeriodRangeRow) rowModel;
				Month start = _controller.getWaterYearDefinition().getStartMonth();
				Month end = _controller.getWaterYearDefinition().getEndMonth();
				boolean overrideWaterYearDefinition = false;
				if(rangeRowModel instanceof WaterYearDefinitionOverrideRow)
				{
					overrideWaterYearDefinition = true;
					start = ((WaterYearDefinitionOverrideRow) rangeRowModel).getStart();
					end = ((WaterYearDefinitionOverrideRow) rangeRowModel).getEnd();
				}

				launchEditAnnualPeriodDialog(selectedItem, rangeRowModel, start, end, overrideWaterYearDefinition);
			}
		}
	}

	private void launchEditAnnualPeriodDialog(TreeItem<WaterYearPeriodDefinitionsRow> selectedItem, WaterYearPeriodRangeRow rangeRowModel, Month start, Month end,
											  boolean overrideWaterYearDefinition)
	{
		AddAnnualFilterDialog addAnnualFilterDialog = new AddAnnualFilterDialog(Frame.getFrames()[0], rangeRowModel.getRange(), start, end, overrideWaterYearDefinition,
				_controller.getWaterYearDefinition());
		SwingUtilities.invokeLater(() ->
		{
			addAnnualFilterDialog.setVisible(true);
			if(!addAnnualFilterDialog.isCanceled())
			{
				int startYear = addAnnualFilterDialog.getStartYear();
				int endYear = addAnnualFilterDialog.getEndYear();
				WaterYearPeriod waterYearPeriod = new WaterYearPeriod("User Defined");
				WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(startYear, waterYearPeriod),
						new WaterYearType(endYear, waterYearPeriod));
				Platform.runLater(()->
				{
					TreeItem<WaterYearPeriodDefinitionsRow> parent = selectedItem.getParent();
					int selectedIndex = parent.getChildren().indexOf(selectedItem);
					int selectedRowIndex = _treeView.getSelectionModel().getSelectedIndex();
					parent.getChildren().remove(selectedItem);
					if(addAnnualFilterDialog.overrideWaterYearDefinition())
					{
						WaterYearDefinitionOverrideRow waterYearPeriodRangeRow = new WaterYearDefinitionOverrideRow(waterYearPeriodRange, addAnnualFilterDialog.getStartMonth(),
								addAnnualFilterDialog.getEndMonth());
						parent.getChildren().add(selectedIndex, new MyCheckBoxTreeItem(waterYearPeriodRangeRow));
					}
					else
					{
						WaterYearPeriodRangeRow waterYearPeriodRangeRow = new WaterYearPeriodRangeRow(waterYearPeriodRange, true);
						parent.getChildren().add(selectedIndex, new MyCheckBoxTreeItem(waterYearPeriodRangeRow));
					}
					_treeView.getSelectionModel().select(selectedRowIndex);
				});
			}
		});
	}

	private void addAnnualPeriod()
	{
		AddAnnualFilterDialog addAnnualFilterDialog = new AddAnnualFilterDialog(Frame.getFrames()[0], _controller.getWaterYearDefinition());
		//Need to invoke later in order to not block the Platform thread on the Swing thread
		SwingUtilities.invokeLater(() ->
		{
			addAnnualFilterDialog.setVisible(true);
			if(!addAnnualFilterDialog.isCanceled())
			{
				int startYear = addAnnualFilterDialog.getStartYear();
				int endYear = addAnnualFilterDialog.getEndYear();
				Platform.runLater(() ->
				{
					TreeItem<WaterYearPeriodDefinitionsRow> root = _treeView.getRoot();
					TreeItem<WaterYearPeriodDefinitionsRow> parent = root.getChildren().get(root.getChildren().size() - 1);
					if(!"User Defined".equals(parent.getValue().toString()))
					{
						parent = new MyCheckBoxTreeItem(new WaterYearPeriodRow(new WaterYearPeriod("User Defined")));
						root.getChildren().add(parent);
					}
					WaterYearPeriod waterYearPeriod = new WaterYearPeriod("User Defined");
					WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(startYear, waterYearPeriod),
							new WaterYearType(endYear, waterYearPeriod));
					if(addAnnualFilterDialog.overrideWaterYearDefinition())
					{
						WaterYearDefinitionOverrideRow waterYearPeriodRangeRow = new WaterYearDefinitionOverrideRow(waterYearPeriodRange,
								addAnnualFilterDialog.getStartMonth(), addAnnualFilterDialog.getEndMonth());
						parent.getChildren().add(new MyCheckBoxTreeItem(waterYearPeriodRangeRow));
					}
					else
					{
						WaterYearPeriodRangeRow waterYearPeriodRangeRow = new WaterYearPeriodRangeRow(waterYearPeriodRange, true);
						parent.getChildren().add(new MyCheckBoxTreeItem(waterYearPeriodRangeRow));
					}
				});
			}
		});
	}

	private void initTree()
	{
		_treeView.setCellFactory(CheckBoxTreeCell.forTreeView());
		_treeView.setShowRoot(false);
		_treeView.setFixedCellSize(20);
		MyCheckBoxTreeItem rootItem = new MyCheckBoxTreeItem(new WaterYearPeriodRow(new WaterYearPeriod("Root")));
		_treeView.setRoot(rootItem);
		_treeView.setCellFactory((TreeView<WaterYearPeriodDefinitionsRow> item) ->
		{
			CheckBoxTreeCell<WaterYearPeriodDefinitionsRow> retval = new CheckBoxTreeCell<>();
			retval.setPrefHeight(20);
			retval.setFont(Font.font(11));
			return retval;
		});
		_treeView.setPrefHeight(415);
		buildWaterYearTypeRangeRows();
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
		List<WaterYearIndexReader.WaterYearIndexDefinition> aliases = WaterYearIndexReader.getInstance().getWaterYearIndexDefinitions();
		for(WaterYearIndexReader.WaterYearIndexDefinition alias : aliases)
		{
			MyCheckBoxTreeItem parent = new MyCheckBoxTreeItem(new WaterYearIndexRow(alias));
			parent.selectedProperty().addListener((e, o, n) -> parentCheckboxSelected(e, o, n, parent));
			root.getChildren().add(parent);
			List<WaterYearPeriod> waterYearPeriods = alias.getWaterYearPeriods();
			waterYearPeriods.stream().map(period -> buildWaterYearIndexTypeRow(alias, period)).forEach(parent.getChildren()::add);
		}
	}

	private MyCheckBoxTreeItem buildWaterYearIndexTypeRow(WaterYearIndexReader.WaterYearIndexDefinition alias, WaterYearPeriod period)
	{
		return new MyCheckBoxTreeItem(new WaterYearIndexTypeRow(alias, period));
	}

	private void buildLongTermPeriod(TreeItem<WaterYearPeriodDefinitionsRow> root)
	{
		root.getChildren().add(new MyCheckBoxTreeItem(new LongTermRow()));
	}

	private void buildContiguousPeriods(TreeItem<WaterYearPeriodDefinitionsRow> root)
	{
		List<WaterYearPeriodReader.WaterYearPeriodDefinition> waterYearPeriodDefinitions = WaterYearPeriodReader.getInstance().getWaterYearPeriodDefinitions();
		List<String> periodNames = waterYearPeriodDefinitions.stream().map(WaterYearPeriodReader.WaterYearPeriodDefinition::getName).distinct().collect(toList());
		for(String period : periodNames)
		{
			MyCheckBoxTreeItem parent = new MyCheckBoxTreeItem(new WaterYearPeriodRow(new WaterYearPeriod(period)));
			parent.selectedProperty().addListener((e, o, n) -> parentCheckboxSelected(e, o, n, parent));
			root.getChildren().add(parent);
			waterYearPeriodDefinitions.stream().filter(s -> s.getName().equalsIgnoreCase(period)).map(this::buildPeriodRow).forEach(parent.getChildren()::add);
		}
	}

	private MyCheckBoxTreeItem buildPeriodRow(WaterYearPeriodReader.WaterYearPeriodDefinition range)
	{
		WaterYearPeriod waterYearPeriod = new WaterYearPeriod(range.getName());
		WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(range.getStartYear(), waterYearPeriod),
				new WaterYearType(range.getEndYear(), waterYearPeriod));
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
			children.stream().filter(child -> child instanceof MyCheckBoxTreeItem).map(child -> (MyCheckBoxTreeItem) child).forEach(child -> child.setSelected(false));
		}
	}

	private List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> getWaterYearPeriodRanges()
	{
		List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> retval = new ArrayList<>();
		for(TreeItem<WaterYearPeriodDefinitionsRow> treeItem : _treeView.getRoot().getChildren())
		{
			if(treeItem instanceof MyCheckBoxTreeItem)
			{
				WaterYearPeriodDefinitionsRow parentValue = treeItem.getValue();
				if(parentValue instanceof LongTermRow && ((MyCheckBoxTreeItem) treeItem).isSelected())
				{
					retval.add(parentValue.getWaterYearPeriodRangesFilter());
				}
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
				if(parentValue instanceof WaterYearIndexRow)
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
		retval.removeIf(Objects::isNull);
		return retval;
	}

	void reloadProject()
	{
		try
		{
			_modifiedListenerEnabled = false;
			List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> waterYearPeriodRanges = _controller.getWaterYearPeriodRanges();
			List<WaterYearPeriodRangesFilter> collect = waterYearPeriodRanges.stream()
																			 .map(Map::entrySet)
																			 .flatMap(Collection::stream)
																			 .map(Map.Entry::getValue)
																			 .collect(toList());
			for(TreeItem<WaterYearPeriodDefinitionsRow> treeItem : _treeView.getRoot().getChildren())
			{
				WaterYearPeriodDefinitionsRow value = treeItem.getValue();
				Map<EpptScenarioRun, WaterYearPeriodRangesFilter> waterYearPeriodRangesFilters = value.getWaterYearPeriodRangesFilter();
				if(waterYearPeriodRangesFilters != null && !waterYearPeriodRangesFilters.isEmpty())
				{
					WaterYearPeriodRangesFilter filter = waterYearPeriodRangesFilters.values().iterator().next();
					String name = filter.getName();
					String groupName = filter.getGroupName();
					((CheckBoxTreeItem<WaterYearPeriodDefinitionsRow>) treeItem).setSelected(
							collect.stream().anyMatch(s -> s.getName().equals(name) && s.getGroupName().equals(groupName)));
				}
				else
				{
					for(TreeItem<WaterYearPeriodDefinitionsRow> child : treeItem.getChildren())
					{
						if(child instanceof CheckBoxTreeItem)
						{
							Collection<WaterYearPeriodRangesFilter> values = child.getValue().getWaterYearPeriodRangesFilter().values();
							if(!values.isEmpty())
							{
								WaterYearPeriodRangesFilter filter = values.iterator().next();
								String name = filter.getName();
								String groupName = filter.getGroupName();
								((CheckBoxTreeItem<WaterYearPeriodDefinitionsRow>) child).setSelected(
										collect.stream().anyMatch(s -> s.getName().equals(name) && s.getGroupName().equals(groupName)));
							}
							else
							{
								((CheckBoxTreeItem<WaterYearPeriodDefinitionsRow>) child).setSelected(false);
							}
						}
					}
				}
			}
			_controller.setWaterYearPeriodRangesFilters(getWaterYearPeriodRanges());
		}
		finally
		{
			_modifiedListenerEnabled = true;
		}
	}

	private interface WaterYearPeriodDefinitionsRow
	{
		String toString();

		default boolean isEditable()
		{
			return false;
		}

		Map<EpptScenarioRun, WaterYearPeriodRangesFilter> getWaterYearPeriodRangesFilter();
	}

	private final class LongTermRow implements WaterYearPeriodDefinitionsRow
	{

		@Override
		public String toString()
		{
			return "Long Term";
		}

		@Override
		public Map<EpptScenarioRun, WaterYearPeriodRangesFilter> getWaterYearPeriodRangesFilter()
		{
			WaterYearPeriod waterYearPeriod = new WaterYearPeriod("Long Term");
			WaterYearType start = new WaterYearType(_controller.getStartYear(), waterYearPeriod);
			WaterYearType end = new WaterYearType(_controller.getEndYear(), waterYearPeriod);
			List<WaterYearPeriodRange> waterYearPeriodRanges = Collections.singletonList(new WaterYearPeriodRange(waterYearPeriod, start, end));
			WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter("", waterYearPeriod.getPeriodName(), waterYearPeriodRanges,
					_controller.getWaterYearDefinition());
			Map<EpptScenarioRun, WaterYearPeriodRangesFilter> retval = new HashMap<>();
			_controller.getScenarioRuns().forEach(s -> retval.put(s, waterYearPeriodRangesFilter));
			return retval;
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
		public Map<EpptScenarioRun, WaterYearPeriodRangesFilter> getWaterYearPeriodRangesFilter()
		{
			return null;
		}
	}

	private static final class WaterYearIndexRow implements WaterYearPeriodDefinitionsRow
	{

		private final WaterYearIndexReader.WaterYearIndexDefinition _alias;

		private WaterYearIndexRow(WaterYearIndexReader.WaterYearIndexDefinition alias)
		{
			_alias = alias;
		}

		@Override
		public String toString()
		{
			return _alias.getDisplayName();
		}

		@Override
		public Map<EpptScenarioRun, WaterYearPeriodRangesFilter> getWaterYearPeriodRangesFilter()
		{
			return null;
		}
	}

	private final class WaterYearIndexTypeRow implements WaterYearPeriodDefinitionsRow
	{
		private final WaterYearIndexReader.WaterYearIndexDefinition _alias;
		private final WaterYearPeriod _period;

		private WaterYearIndexTypeRow(WaterYearIndexReader.WaterYearIndexDefinition alias, WaterYearPeriod period)
		{
			_alias = alias;
			_period = period;
		}

		@Override
		public String toString()
		{
			return _period.getPeriodName();
		}

		@Override
		public Map<EpptScenarioRun, WaterYearPeriodRangesFilter> getWaterYearPeriodRangesFilter()
		{
			Map<EpptScenarioRun, WaterYearPeriodRangesFilter> retval = new HashMap<>();
			WaterYearPeriod waterYearPeriod = new WaterYearPeriod(_period.getPeriodName());
			try
			{
				for(EpptScenarioRun scenarioRun : _controller.getScenarioRuns())
				{
					WaterYearTableReader waterYearTableReader = new WaterYearTableReader(scenarioRun);
					List<WaterYearIndexModel> read = waterYearTableReader.read();
					Optional<WaterYearIndexModel> indexOpt = read.stream().filter(_alias::matchesModel).findAny();
					if(indexOpt.isPresent())
					{
						List<WaterYearPeriodRange> waterYearPeriodRanges = indexOpt.get()
																				   .getWaterYearTypes()
																				   .stream()
																				   .filter(s -> s.getWaterYearPeriod().equals(_period))
																				   .map(type -> new WaterYearPeriodRange(type.getWaterYearPeriod(), type, type))
																				   .collect(toList());
						WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter(waterYearPeriod.getPeriodName(), _alias.getDisplayName(),
								waterYearPeriodRanges, _controller.getWaterYearDefinition());
						retval.put(scenarioRun, waterYearPeriodRangesFilter);
					}
					else
					{
						LOGGER.log(Level.WARNING, "No Water Year Index definition found for Water Year Index: {0} for scenario: {1}", new Object[]{_alias, scenarioRun});
						WaterYearPeriodRange placeholderPeriod = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(0, waterYearPeriod),
								new WaterYearType(0, waterYearPeriod));
						WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter(waterYearPeriod.getPeriodName(), _alias.getDisplayName(),
								Collections.singletonList(placeholderPeriod), _controller.getWaterYearDefinition());
						retval.put(scenarioRun, waterYearPeriodRangesFilter);
					}
				}
			}
			catch(EpptInitializationException e)
			{
				LOGGER.log(Level.SEVERE, "Error reading lookup directory, cannot add period: " + waterYearPeriod, e);
				retval.clear();
			}
			return retval;
		}
	}

	private class WaterYearPeriodRangeRow implements WaterYearPeriodDefinitionsRow
	{
		private final WaterYearPeriodRange _range;
		private final boolean _editable;

		private WaterYearPeriodRangeRow(WaterYearPeriodRange range)
		{
			_range = range;
			_editable = false;
		}

		private WaterYearPeriodRangeRow(WaterYearPeriodRange range, boolean editable)
		{
			_range = range;
			_editable = editable;
		}

		WaterYearPeriodRange getRange()
		{
			return _range;
		}

		@Override
		public boolean isEditable()
		{
			return _editable;
		}

		@Override
		public String toString()
		{
			DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MMM yyyy");
			return _range.toString(_controller.getWaterYearDefinition(), formatter);
		}

		@Override
		public Map<EpptScenarioRun, WaterYearPeriodRangesFilter> getWaterYearPeriodRangesFilter()
		{
			List<WaterYearPeriodRange> waterYearPeriodRanges = Collections.singletonList(_range);
			String periodName = _range.getWaterYearPeriod().getPeriodName();
			String rangeName = _range.toString(null, DateTimeFormatter.ofPattern("MMM yyyy"));
			WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter(rangeName, periodName, waterYearPeriodRanges,
					_controller.getWaterYearDefinition());
			Map<EpptScenarioRun, WaterYearPeriodRangesFilter> retval = new HashMap<>();
			_controller.getScenarioRuns().forEach(s -> retval.put(s, waterYearPeriodRangesFilter));
			return retval;
		}
	}

	private final class WaterYearDefinitionOverrideRow extends WaterYearPeriodRangeRow
	{
		private final WaterYearPeriodRange _range;
		private final Month _start;
		private final Month _end;

		private WaterYearDefinitionOverrideRow(WaterYearPeriodRange range, Month start, Month end)
		{
			super(range);
			_range = range;
			_start = start;
			_end = end;
		}

		Month getStart()
		{
			return _start;
		}

		Month getEnd()
		{
			return _end;
		}

		@Override
		public boolean isEditable()
		{
			return true;
		}

		@Override
		public String toString()
		{
			DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MMM yyyy");
			YearMonth start = YearMonth.of(_range.getStartYear().getYear(), _start);
			YearMonth end = YearMonth.of(_range.getEndYear().getYear(), _end);
			return formatter.format(start) + " - " + formatter.format(end);
		}

		@Override
		public Map<EpptScenarioRun, WaterYearPeriodRangesFilter> getWaterYearPeriodRangesFilter()
		{
			List<WaterYearPeriodRange> waterYearPeriodRanges = Collections.singletonList(_range);
			String periodName = _range.getWaterYearPeriod().getPeriodName();
			String rangeName = _range.toString(null, DateTimeFormatter.ofPattern("MMM yyyy"));
			WaterYearPeriodRangesFilter waterYearPeriodRangesFilter = new WaterYearPeriodRangesFilter(rangeName, periodName, waterYearPeriodRanges,
					new WaterYearDefinition(_range.getWaterYearPeriod().getPeriodName(), _start, _end, 1, 1));
			Map<EpptScenarioRun, WaterYearPeriodRangesFilter> retval = new HashMap<>();
			_controller.getScenarioRuns().forEach(s -> retval.put(s, waterYearPeriodRangesFilter));
			return retval;
		}
	}

	private final class MyCheckBoxTreeItem extends CheckBoxTreeItem<WaterYearPeriodDefinitionsRow>
	{
		private MyCheckBoxTreeItem(WaterYearPeriodDefinitionsRow row)
		{
			super(row);
			selectedProperty().addListener((e, o, n) ->
			{
				if(_modifiedListenerEnabled)
				{
					_controller.setWaterYearPeriodRangesFilters(getWaterYearPeriodRanges());
					_controller.setModified();
				}
			});
		}
	}


}

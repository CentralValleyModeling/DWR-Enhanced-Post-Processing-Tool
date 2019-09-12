/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.quickresults.ui.report;

import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.CheckBoxTreeItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.control.cell.CheckBoxTreeCell;
import javafx.scene.layout.BorderPane;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-20-2019
 */
public class WaterYearPeriodsPanel extends JFXPanel
{
	private static final int MAX_PERIODS = 5;
	private TreeView<WaterYearPeriodDefinitionsRow> _treeView;
	private WaterYearDefinition _waterYearDefinition;

	WaterYearPeriodsPanel()
	{
		Platform.setImplicitExit(false);
		Platform.runLater(this::init);
	}

	private void init()
	{
		_treeView = new TreeView<>();
		_treeView.setCellFactory(CheckBoxTreeCell.forTreeView());
		_treeView.setShowRoot(false);
		CheckBoxTreeItem<WaterYearPeriodDefinitionsRow> rootItem = new CheckBoxTreeItem<>(new WaterYearPeriodRow(new WaterYearPeriod("Root")));
		_treeView.setRoot(rootItem);
		BorderPane borderPane = new BorderPane();
		borderPane.setCenter(_treeView);
		setScene(new Scene(borderPane));
		_treeView.setCellFactory((TreeView<WaterYearPeriodDefinitionsRow> item) -> {

			final CheckBoxTreeCell<WaterYearPeriodDefinitionsRow> cell = new CheckBoxTreeCell<>();

			cell.itemProperty().addListener((obs, s, s1) ->
			{
				cell.disableProperty().unbind();
				if(s1 instanceof WaterYearPeriodRangeRow)
				{
					cell.disableProperty().bind(((WaterYearPeriodRangeRow) s1).disabledProperty());
				}
			});
			return cell;
		});
	}

	void fillWithIndex(WaterYearIndex waterYearIndex, WaterYearDefinition waterYearDefinition)
	{
		Platform.runLater(() ->
		{
			_waterYearDefinition = waterYearDefinition;
			_treeView.getRoot().getChildren().clear();
			buildWaterYearTypeRangeRows(waterYearIndex);
		});
	}

	void fillWithDefinition(WaterYearDefinition waterYearDefinition)
	{
		Platform.runLater(() ->
		{
			_waterYearDefinition = waterYearDefinition;
			_treeView.refresh();
		});
	}

	private void buildWaterYearTypeRangeRows(WaterYearIndex waterYearIndex)
	{
		TreeItem<WaterYearPeriodDefinitionsRow> root = _treeView.getRoot();
		for(Map.Entry<WaterYearPeriod, List<WaterYearPeriodRange>> entry : waterYearIndex.getWaterYearPeriodRanges().entrySet())
		{
			CheckBoxTreeItem<WaterYearPeriodDefinitionsRow> parent = new CheckBoxTreeItem<>(new WaterYearPeriodRow(entry.getKey()),
					null, false, true);
			parent.selectedProperty().addListener((e, o, n) -> parentCheckboxSelected(e, o, n, parent));
			root.getChildren().add(parent);
			List<WaterYearPeriodRange> ranges = entry.getValue();
			SimpleIntegerProperty totalSelectedProperty = new SimpleIntegerProperty(0);
			ranges.stream()
				  .map(e -> buildPeriodRow(e, totalSelectedProperty))
				  .forEach(parent.getChildren()::add);
		}
	}

	private CheckBoxTreeItem<WaterYearPeriodDefinitionsRow> buildPeriodRow(WaterYearPeriodRange range,
																		   SimpleIntegerProperty totalSelectedProperty)
	{
		WaterYearPeriodRangeRow waterYearPeriodRangeRow = new WaterYearPeriodRangeRow(range);
		CheckBoxTreeItem<WaterYearPeriodDefinitionsRow> retval = new CheckBoxTreeItem<>(waterYearPeriodRangeRow);
		retval.selectedProperty().addListener((e, o, n) ->
		{
			if(n)
			{
				totalSelectedProperty.set(totalSelectedProperty.getValue() + 1);
			}
			else
			{
				totalSelectedProperty.set(Math.max(0, totalSelectedProperty.getValue() - 1));
			}
		});
		totalSelectedProperty.addListener(
				(e, o, n) -> waterYearPeriodRangeRow.disabledProperty().setValue(!retval.isSelected() && n.intValue() >= MAX_PERIODS));
		return retval;
	}

	private void parentCheckboxSelected(ObservableValue<? extends Boolean> e, Boolean o, Boolean selected,
										CheckBoxTreeItem<WaterYearPeriodDefinitionsRow> parent)
	{
		ObservableList<TreeItem<WaterYearPeriodDefinitionsRow>> children = parent.getChildren();
		if(selected)
		{
			int alreadySelected = (int) children.stream().filter(child -> child instanceof CheckBoxTreeItem)
												.map(child -> (CheckBoxTreeItem) child)
												.filter(CheckBoxTreeItem::isSelected)
												.count();
			for(TreeItem<WaterYearPeriodDefinitionsRow> child : children)
			{
				if(child instanceof CheckBoxTreeItem && alreadySelected < MAX_PERIODS)
				{
					CheckBoxTreeItem<WaterYearPeriodDefinitionsRow> checkBoxTreeItem = (CheckBoxTreeItem<WaterYearPeriodDefinitionsRow>) child;
					if(!checkBoxTreeItem.isSelected())
					{
						checkBoxTreeItem.setSelected(true);
						alreadySelected++;
					}
				}
			}
		}
		else
		{
			children.stream().filter(child -> child instanceof CheckBoxTreeItem)
					.map(child -> (CheckBoxTreeItem) child)
					.forEach(child -> child.setSelected(false));
		}
	}

	Map<WaterYearPeriod, List<WaterYearPeriodRange>> getWaterYearPeriodRanges()
	{
		Map<WaterYearPeriod, List<WaterYearPeriodRange>> retval = new HashMap<>();
		for(TreeItem<WaterYearPeriodDefinitionsRow> treeItem : _treeView.getRoot().getChildren())
		{
			WaterYearPeriodDefinitionsRow parentValue = treeItem.getValue();
			if(parentValue instanceof WaterYearPeriodRow)
			{
				List<WaterYearPeriodRange> ranges = new ArrayList<>();
				for(TreeItem<WaterYearPeriodDefinitionsRow> child : treeItem.getChildren())
				{
					WaterYearPeriodDefinitionsRow childValue = child.getValue();
					if(childValue instanceof WaterYearPeriodRangeRow)
					{
						ranges.add(((WaterYearPeriodRangeRow) childValue)._range);
					}
				}
				if(!ranges.isEmpty())
				{
					retval.put(((WaterYearPeriodRow)parentValue)._period, ranges);
				}
			}
		}
		return retval;
	}

	private interface WaterYearPeriodDefinitionsRow
	{
		String toString();
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
	}

	private final class WaterYearPeriodRangeRow implements WaterYearPeriodDefinitionsRow
	{
		private final BooleanProperty _disabledProperty = new SimpleBooleanProperty(false);
		private final WaterYearPeriodRange _range;

		private WaterYearPeriodRangeRow(WaterYearPeriodRange range)
		{
			_range = range;
		}

		@Override
		public String toString()
		{
			DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MMM yy");
			return _range.toString(_waterYearDefinition, formatter);

		}

		private BooleanProperty disabledProperty()
		{
			return _disabledProperty;
		}
	}
}

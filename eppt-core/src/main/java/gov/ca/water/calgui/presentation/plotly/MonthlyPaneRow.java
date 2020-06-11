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

package gov.ca.water.calgui.presentation.plotly;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import gov.ca.water.calgui.constant.Constant;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.paint.Color;

import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;
import com.rma.javafx.treetable.rows.RmaTreeTableRowModel;

import static java.util.stream.Collectors.toMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
abstract class MonthlyPaneRow extends RmaTreeTableRowModel<MonthlyPaneRow>
{

	MonthlyPaneRow(MonthlyPaneRow parent)
	{
		super(parent);
	}

	@Override
	public boolean isEditable(TreeTableColumnSpec columnSpec)
	{
		return false;
	}

	@Override
	public Color getUneditableBgCellColor(TreeTableColumnSpec columnSpec)
	{
		return getEditableBgCellColor(columnSpec);
	}

	static class MonthlyPaneRowScenario extends MonthlyPaneRow
	{
		private final int _index;
		private final SimpleStringProperty _title;
		private final TreeTableColumnSpec _treeTableColumnSpec;

		MonthlyPaneRowScenario(int index, String title, TreeTableColumnSpec treeTableColumnSpec)
		{
			super(null);
			_index = index;
			_title = new SimpleStringProperty(title.replace("<br>", " "));
			_treeTableColumnSpec = treeTableColumnSpec;
		}

		String getTitle()
		{
			return _title.get();
		}

		int getIndex()
		{
			return _index;
		}

		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == _treeTableColumnSpec)
			{
				return _title;
			}
			return null;
		}

		@Override
		public boolean equals(Object o)
		{
			if(this == o)
			{
				return true;
			}
			if(o == null || getClass() != o.getClass())
			{
				return false;
			}
			final MonthlyPaneRowScenario that = (MonthlyPaneRowScenario) o;
			return _title.get().equals(that._title.get());
		}

		@Override
		public int hashCode()
		{
			return Objects.hash(_title.get());
		}
	}

	static class MonthlyPaneRowData extends MonthlyPaneRow
	{

		private final SimpleStringProperty _year;
		private final Map<TreeTableColumnSpec, SimpleStringProperty> _data = new HashMap<>();
		private final TreeTableColumnSpec _treeTableColumnSpec;

		MonthlyPaneRowData(String header, TreeTableColumnSpec treeTableColumnSpec)
		{
			super(null);
			_year = new SimpleStringProperty(header);
			_treeTableColumnSpec = treeTableColumnSpec;
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == _treeTableColumnSpec)
			{
				return _year;
			}
			return _data.get(treeTableColumnSpec);
		}

		void addData(Map<TreeTableColumnSpec, Double> dataMap)
		{
			_data.putAll(dataMap.entrySet().stream().collect(toMap(Map.Entry::getKey, e -> Constant.getStringForDouble(e.getValue()))));
		}
	}
}

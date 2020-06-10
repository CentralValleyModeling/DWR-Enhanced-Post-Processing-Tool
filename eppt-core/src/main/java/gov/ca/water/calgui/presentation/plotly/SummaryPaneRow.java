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

import java.util.Map;

import gov.ca.water.calgui.busservice.impl.EpptStatistic;
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
abstract class SummaryPaneRow extends RmaTreeTableRowModel<SummaryPaneRow>
{

	public SummaryPaneRow(SummaryPaneRow parent)
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

	static class SummaryPaneRowScenario extends SummaryPaneRow
	{
		private final SimpleStringProperty _title;
		private final TreeTableColumnSpec _treeTableColumnSpec;

		SummaryPaneRowScenario(String title, TreeTableColumnSpec treeTableColumnSpec)
		{
			super(null);
			_title = new SimpleStringProperty(title.replace("<br>", " "));
			_treeTableColumnSpec = treeTableColumnSpec;
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
	}

	static class SummaryPaneRowStat extends SummaryPaneRow
	{
		private final SimpleStringProperty _title;
		private final TreeTableColumnSpec _treeTableColumnSpec;

		SummaryPaneRowStat(SummaryPaneRow parent, EpptStatistic epptStatistic, TreeTableColumnSpec treeTableColumnSpec)
		{
			super(parent);
			_title = new SimpleStringProperty(epptStatistic.getName());
			_treeTableColumnSpec = treeTableColumnSpec;
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
	}

	static class SummaryPaneRowData extends SummaryPaneRow
	{
		private final Map<TreeTableColumnSpec, SimpleStringProperty> _data;
		private final TreeTableColumnSpec _rootSpec;
		private final SimpleStringProperty _rowTitle;

		SummaryPaneRowData(SummaryPaneRow parent, String rowTitle, Map<TreeTableColumnSpec, Double> data, TreeTableColumnSpec rootSpec)
		{
			super(parent);
			_rowTitle = new SimpleStringProperty(rowTitle);
			_data = data.entrySet().stream().collect(toMap(Map.Entry::getKey, e ->Constant.getStringForDouble(e.getValue())));
			_rootSpec = rootSpec;
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == _rootSpec)
			{
				return _rowTitle;
			}
			return _data.get(treeTableColumnSpec);
		}
	}
}

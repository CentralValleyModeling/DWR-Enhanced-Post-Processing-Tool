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

import java.math.BigDecimal;
import java.math.MathContext;
import java.text.DecimalFormat;
import java.util.Map;

import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.paint.Color;

import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;
import com.rma.javafx.treetable.rows.RmaTreeTableRowModel;

import static gov.ca.water.calgui.presentation.plotly.SummaryTableModel.ROOT_COL_SPEC;
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

		SummaryPaneRowScenario(String title)
		{
			super(null);
			_title = new SimpleStringProperty(title);
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == ROOT_COL_SPEC)
			{
				return _title;
			}
			return null;
		}
	}

	static class SummaryPaneRowStat extends SummaryPaneRow
	{
		private final SimpleStringProperty _title;

		SummaryPaneRowStat(SummaryPaneRow parent, EpptStatistic epptStatistic)
		{
			super(parent);
			_title = new SimpleStringProperty(epptStatistic.getName());
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == ROOT_COL_SPEC)
			{
				return _title;
			}
			return null;
		}
	}
	static class SummaryPaneBlankRow extends SummaryPaneRow
	{

		public SummaryPaneBlankRow(SummaryPaneRow parent)
		{
			super(parent);
		}

		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			return null;
		}
	}

	static class SummaryPaneRowWaterYearIndex extends SummaryPaneRow
	{
		private final SimpleStringProperty _title;

		SummaryPaneRowWaterYearIndex(SummaryPaneRow parent, EpptStatistic key,
									 WaterYearIndex waterYearIndex)
		{
			super(parent);
			_title = new SimpleStringProperty(key + " - " + waterYearIndex.getName());
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == ROOT_COL_SPEC)
			{
				return _title;
			}
			return null;
		}
	}

	static class SummaryPaneRowData extends SummaryPaneRow
	{
		private final Map<TreeTableColumnSpec, SimpleStringProperty> _data;
		private final SimpleStringProperty _rowTitle;

		SummaryPaneRowData(SummaryPaneRow parent, String rowTitle, Map<TreeTableColumnSpec, Double> data)
		{
			super(parent);
			_rowTitle = new SimpleStringProperty(rowTitle);
			_data = data.entrySet().stream().collect(toMap(Map.Entry::getKey,
					e ->
					{
						Double value = e.getValue();
						if(value != null && !value.isNaN())
						{
							BigDecimal bigDecimal = BigDecimal.valueOf(value);
							bigDecimal = bigDecimal.round(new MathContext(3));
							return new SimpleStringProperty(bigDecimal.toPlainString());
						}
						else
						{
							return new SimpleStringProperty("");
						}
					}));
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == ROOT_COL_SPEC)
			{
				return _rowTitle;
			}
			return _data.get(treeTableColumnSpec);
		}
	}
}

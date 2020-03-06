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
import java.util.Map;

import gov.ca.water.calgui.busservice.impl.EpptStatistic;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.paint.Color;

import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;
import com.rma.javafx.treetable.rows.RmaTreeTableRowModel;

import static gov.ca.water.calgui.presentation.plotly.MonthlyTableModel.WATER_YEAR_COL_SPEC;
import static java.util.stream.Collectors.toMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
abstract class MonthlyPaneRow extends RmaTreeTableRowModel<MonthlyPaneRow>
{

	public MonthlyPaneRow(MonthlyPaneRow parent)
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
		private final SimpleStringProperty _title;

		MonthlyPaneRowScenario(String title)
		{
			super(null);
			_title = new SimpleStringProperty(title);
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == WATER_YEAR_COL_SPEC)
			{
				return _title;
			}
			return null;
		}
	}

	static class MonthlyPaneRowData extends MonthlyPaneRow
	{

		private final SimpleStringProperty _year;
		private final Map<TreeTableColumnSpec, SimpleStringProperty> _data;

		MonthlyPaneRowData(MonthlyPaneRowScenario parent, int year, Map<TreeTableColumnSpec, Double> data)
		{
			super(parent);
			_year = new SimpleStringProperty(Integer.toString(year));
			_data = data.entrySet().stream().collect(toMap(Map.Entry::getKey,
					e->
					{
						BigDecimal bigDecimal = BigDecimal.valueOf(e.getValue());
						bigDecimal = bigDecimal.round(new MathContext(3));
						return new SimpleStringProperty(Double.toString(bigDecimal.doubleValue()));
					}));
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == WATER_YEAR_COL_SPEC)
			{
				return _year;
			}
			return _data.get(treeTableColumnSpec);
		}
	}

	static class MonthlyPaneRowStat extends MonthlyPaneRow
	{

		private final SimpleStringProperty _stat;
		private final Map<TreeTableColumnSpec, SimpleStringProperty> _data;

		MonthlyPaneRowStat(MonthlyPaneRowScenario parent, EpptStatistic epptStatistic, Map<TreeTableColumnSpec, Double> data)
		{
			super(parent);
			_stat = new SimpleStringProperty(epptStatistic.getName());
			_data = data.entrySet().stream().collect(toMap(Map.Entry::getKey,
					e->
					{
						BigDecimal bigDecimal = BigDecimal.valueOf(e.getValue());
						bigDecimal = bigDecimal.round(new MathContext(3));
						return new SimpleStringProperty(Double.toString(bigDecimal.doubleValue()));
					}));
		}


		@Override
		public ObservableValue<?> getObservableValue(TreeTableColumnSpec treeTableColumnSpec)
		{
			if(treeTableColumnSpec == WATER_YEAR_COL_SPEC)
			{
				return _stat;
			}
			return _data.get(treeTableColumnSpec);
		}
	}
}

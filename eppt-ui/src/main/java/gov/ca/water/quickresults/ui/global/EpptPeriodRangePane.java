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

import java.time.Month;
import java.time.YearMonth;
import java.time.format.TextStyle;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.stream.IntStream;

import com.sun.javafx.scene.control.skin.TableViewSkin;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.bo.WaterYearPeriodRangeFilter;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.bo.WaterYearType;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.layout.BorderPane;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 06-18-2020
 */
public class EpptPeriodRangePane extends BorderPane
{
	private final TableView<PeriodRangeRow> _tableView;
	private final TableColumn<PeriodRangeRow, String> _waterYearColumn;
	private final WaterYearDefinition _waterYearDefinition;
	private final TableColumn<PeriodRangeRow, String> _calendarYearColumn;
	private final Map<TableColumn<PeriodRangeRow, String>, Month> _monthColumns = new HashMap<>();
	private WaterYearPeriodRange _waterYearPeriodRange;
	private WaterYearPeriodRangesFilter _waterYearPeriodRangesFilter;

	public EpptPeriodRangePane(WaterYearDefinition waterYearDefinition)
	{
		_waterYearDefinition = waterYearDefinition;
		_tableView = new TableView<>();
		setCenter(_tableView);
		_waterYearColumn = new TableColumn<>("WY");
		_waterYearColumn.setStyle( "-fx-alignment: CENTER;");
		_waterYearColumn.setCellValueFactory(param -> getValueForColumn(param, _waterYearColumn));
		_tableView.getColumns().add(_waterYearColumn);
		for(int i = 0; i < 12; i++)
		{
			Month month = _waterYearDefinition.getStartMonth().plus(i);
			TableColumn<PeriodRangeRow, String> tableColumn = new TableColumn<>(month.getDisplayName(TextStyle.SHORT, Locale.getDefault()));
			tableColumn.setCellValueFactory(param -> getValueForColumn(param, tableColumn));
			tableColumn.setStyle( "-fx-alignment: CENTER;");
			_monthColumns.put(tableColumn, month);
			_tableView.getColumns().add(tableColumn);
		}
		_calendarYearColumn = new TableColumn<>("CY");
		_calendarYearColumn.setStyle( "-fx-alignment: CENTER;");
		_calendarYearColumn.setCellValueFactory(param -> getValueForColumn(param, _calendarYearColumn));
		_tableView.getColumns().add(_calendarYearColumn);
		_tableView.setPrefSize(850, 150);
		_tableView.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
	}

	private SimpleStringProperty getValueForColumn(TableColumn.CellDataFeatures<PeriodRangeRow, String> param, TableColumn<PeriodRangeRow, String> column)
	{
		PeriodRangeRow value = param.getValue();
		if(value != null)
		{
			return value.getValue(column);
		}
		return null;
	}

	public void fill(WaterYearPeriodRangesFilter waterYearPeriodRangesFilter)
	{
		_waterYearPeriodRangesFilter = waterYearPeriodRangesFilter;
		List<WaterYearPeriodRange> waterYearPeriodRanges = _waterYearPeriodRangesFilter.getWaterYearPeriodRanges();
		_waterYearPeriodRange = waterYearPeriodRanges.get(0);
		ObservableList<PeriodRangeRow> rows = FXCollections.observableArrayList();
		rows.add(buildFirstRow());
		int waterYearsBetween = _waterYearPeriodRange.getEndYear().getYear() - _waterYearPeriodRange.getStartYear().getYear();
		if(waterYearsBetween > 1)
		{
			rows.add(buildNextRow());
		}
		if(waterYearsBetween > 3)
		{
			rows.add(new EllipsisRow());
		}
		if(waterYearsBetween > 2)
		{
			rows.add(buildPreviousRow());
		}
		if(waterYearsBetween > 0)
		{
			rows.add(buildLastRow());
		}
		_tableView.getItems().setAll(rows);
		_tableView.refresh();
	}

	private PeriodRangeRow buildPreviousRow()
	{
		WaterYearPeriodRange waterYearPeriodRange = _waterYearPeriodRangesFilter.getWaterYearPeriodRanges().get(0);
		YearMonth endOfLastYear = waterYearPeriodRange.getEnd(_waterYearPeriodRangesFilter.getWaterYearDefinition());
		MonthPeriod monthPeriod = new MonthPeriod("", _waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth());
		int waterYear = monthPeriod.getWaterYear(endOfLastYear) - 1;
		List<YearMonth> yearMonths = monthPeriod.getYearMonths(waterYear);
		return new DataRow(waterYear, yearMonths.get(0).getYear(), yearMonths.get(yearMonths.size() - 1).getYear());
	}

	private PeriodRangeRow buildNextRow()
	{
		WaterYearPeriodRange waterYearPeriodRange = _waterYearPeriodRangesFilter.getWaterYearPeriodRanges().get(0);
		YearMonth startOfFirstYear = waterYearPeriodRange.getStart(_waterYearPeriodRangesFilter.getWaterYearDefinition());
		MonthPeriod monthPeriod = new MonthPeriod("", _waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth());
		int waterYear = monthPeriod.getWaterYear(startOfFirstYear) + 1;
		List<YearMonth> yearMonths = monthPeriod.getYearMonths(waterYear);
		return new DataRow(waterYear, yearMonths.get(0).getYear(), yearMonths.get(yearMonths.size() - 1).getYear());
	}

	private DataRow buildLastRow()
	{
		WaterYearPeriodRange waterYearPeriodRange = _waterYearPeriodRangesFilter.getWaterYearPeriodRanges().get(0);
		YearMonth endOfLastYear = waterYearPeriodRange.getEnd(_waterYearPeriodRangesFilter.getWaterYearDefinition());
		MonthPeriod monthPeriod = new MonthPeriod("", _waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth());
		int waterYear = monthPeriod.getWaterYear(endOfLastYear);
		List<YearMonth> yearMonths = monthPeriod.getYearMonths(waterYear);
		return new DataRow(waterYear, yearMonths.get(0).getYear(), yearMonths.get(yearMonths.size() - 1).getYear());
	}

	private DataRow buildFirstRow()
	{
		WaterYearPeriodRange waterYearPeriodRange = _waterYearPeriodRangesFilter.getWaterYearPeriodRanges().get(0);
		YearMonth startOfFirstYear = waterYearPeriodRange.getStart(_waterYearPeriodRangesFilter.getWaterYearDefinition());
		MonthPeriod monthPeriod = new MonthPeriod("", _waterYearDefinition.getStartMonth(), _waterYearDefinition.getEndMonth());
		int waterYear = monthPeriod.getWaterYear(startOfFirstYear);
		List<YearMonth> yearMonths = monthPeriod.getYearMonths(waterYear);
		return new DataRow(waterYear, yearMonths.get(0).getYear(), yearMonths.get(yearMonths.size() - 1).getYear());
	}

	private interface PeriodRangeRow
	{
		SimpleStringProperty getValue(TableColumn<PeriodRangeRow, String> tableColumn);
	}

	private final class DataRow implements PeriodRangeRow
	{
		private final SimpleStringProperty _xMarkProperty = new SimpleStringProperty("x");
		private final SimpleStringProperty _waterYearProperty;
		private final int _waterYear;
		private final SimpleStringProperty _calendarYearProperty;

		private DataRow(int waterYear, int calendarYearStart, int calendarYearEnd)
		{
			_waterYearProperty = new SimpleStringProperty(Integer.toString(waterYear));
			_waterYear = waterYear;
			if(calendarYearStart == calendarYearEnd)
			{
				_calendarYearProperty = new SimpleStringProperty(calendarYearStart + "");
			}
			else
			{
				_calendarYearProperty = new SimpleStringProperty(calendarYearStart + " - " + calendarYearEnd);
			}
		}

		@Override
		public SimpleStringProperty getValue(TableColumn<PeriodRangeRow, String> tableColumn)
		{
			SimpleStringProperty retval = null;
			if(tableColumn == _waterYearColumn)
			{
				retval = _waterYearProperty;
			}
			else if(tableColumn == _calendarYearColumn)
			{
				retval = _calendarYearProperty;
			}
			else if(canDisplay(_monthColumns.get(tableColumn), _waterYear))
			{
				retval = _xMarkProperty;
			}
			return retval;
		}

		private boolean canDisplay(Month month, int waterYear)
		{
			boolean retval = false;
			if(_waterYearPeriodRangesFilter != null)
			{
				WaterYearPeriod waterYearPeriod = new WaterYearPeriod(waterYear + "");
				WaterYearPeriodRange waterYearPeriodRange = new WaterYearPeriodRange(waterYearPeriod, new WaterYearType(waterYear, waterYearPeriod),
						new WaterYearType(waterYear, waterYearPeriod));
				YearMonth start = waterYearPeriodRange.getStart(_waterYearDefinition);
				Optional<YearMonth> yearMonth = IntStream.iterate(0, i -> i + 1).limit(12).mapToObj(start::plusMonths).filter(m -> m.getMonth() == month).findFirst();
				if(yearMonth.isPresent())
				{
					retval = _waterYearPeriodRangesFilter.testYearMonth(yearMonth.get());
				}
			}
			return retval;
		}
	}

	private static class EllipsisRow implements PeriodRangeRow
	{
		private final SimpleStringProperty _simpleStringProperty = new SimpleStringProperty("...");

		@Override
		public SimpleStringProperty getValue(TableColumn<PeriodRangeRow, String> tableColumn)
		{
			return _simpleStringProperty;
		}
	}
}

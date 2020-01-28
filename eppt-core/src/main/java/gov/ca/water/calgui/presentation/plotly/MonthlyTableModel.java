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

import java.time.LocalDateTime;
import java.time.Month;
import java.time.YearMonth;
import java.time.format.TextStyle;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;

import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputed;
import gov.ca.water.calgui.busservice.impl.EpptReportingComputedSet;
import gov.ca.water.calgui.busservice.impl.EpptReportingScenarioComputed;
import gov.ca.water.calgui.busservice.impl.MonthPeriod;
import gov.ca.water.calgui.project.EpptScenarioRun;

import com.rma.javafx.treetable.RmaTreeTableModel;
import com.rma.javafx.treetable.columns.specs.RmaTreeTableColumnSpec;
import com.rma.javafx.treetable.columns.specs.TreeTableColumnSpec;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-27-2020
 */
class MonthlyTableModel extends RmaTreeTableModel<MonthlyPaneRow>
{
	static final RmaTreeTableColumnSpec WATER_YEAR_COL_SPEC;

	static
	{
		WATER_YEAR_COL_SPEC = new RmaTreeTableColumnSpec.Builder("WY")
				.withCanBeHidden(false)
				.withEditable(false)
				.withSortable(false)
				.withVisibleByDefault(true)
				.build();
	}

	MonthlyTableModel(String plotTitle, WaterYearDefinition waterYearDefinition, EpptReportingComputedSet epptReportingComputedSet)
	{
		Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap = processColumns(waterYearDefinition);
		MonthPeriod monthPeriod = new MonthPeriod(waterYearDefinition.getStartMonth(), waterYearDefinition.getEndMonth());
		List<EpptReportingScenarioComputed> epptReportingComputed = epptReportingComputedSet.getEpptReportingComputed();
		for(EpptReportingScenarioComputed computed : epptReportingComputed)
		{
			processRows(monthTreeTableColumnSpecMap, monthPeriod, plotTitle, computed.getEpptScenarioRun(), computed.getPrimary());
			processRows(monthTreeTableColumnSpecMap, monthPeriod, plotTitle, computed.getEpptScenarioRun(), computed.getSecondary());
		}
	}

	private void processRows(Map<Month, TreeTableColumnSpec> monthTreeTableColumnSpecMap, MonthPeriod monthPeriod,
							 String plotTitle, EpptScenarioRun epptScenarioRun, EpptReportingComputed primary)
	{
		List<SortedMap<LocalDateTime, Double>> fullTimeSeries = primary.getFullTimeSeries();
		List<String> dataSuffix = primary.getDataSuffix();
		for(int i = 0; i < fullTimeSeries.size(); i++)
		{
			String title = epptScenarioRun.getName();
			if(i < dataSuffix.size())
			{
				title += " - " + dataSuffix.get(i);
			}
			else
			{
				title += " - " + plotTitle;
			}
			SortedMap<LocalDateTime, Double> series = fullTimeSeries.get(i);
			if(!series.isEmpty())
			{
				MonthlyPaneRow.MonthlyPaneRowScenario monthlyPaneRowScenario = new MonthlyPaneRow.MonthlyPaneRowScenario(title);
				getRows().add(monthlyPaneRowScenario);
				for(int year = series.firstKey().getYear(); year < series.lastKey().getYear(); year++)
				{
					List<YearMonth> yearMonths = monthPeriod.getYearMonths(year);
					LocalDateTime startYearMonth = yearMonths.get(0).atEndOfMonth().minusDays(2).atTime(0, 0);
					LocalDateTime endYearMonth = yearMonths.get(yearMonths.size() - 1).atEndOfMonth().plusDays(2).atTime(0, 0);
					if(series.firstKey().isAfter(startYearMonth) || series.lastKey().isBefore(endYearMonth))
					{
						continue;
					}
					SortedMap<LocalDateTime, Double> yearValues = series.subMap(startYearMonth, endYearMonth);
					Map<TreeTableColumnSpec, Double> dataMap = new HashMap<>();
					for(Map.Entry<LocalDateTime, Double> entry : yearValues.entrySet())
					{
						LocalDateTime localDateTime = entry.getKey().minusMonths(1);
						dataMap.put(monthTreeTableColumnSpecMap.get(localDateTime.getMonth()), entry.getValue());
					}
					MonthlyPaneRow.MonthlyPaneRowData dataRow = new MonthlyPaneRow.MonthlyPaneRowData(monthlyPaneRowScenario, year, dataMap);
					monthlyPaneRowScenario.getChildren().add(dataRow);
				}
			}

		}
	}

	private Map<Month, TreeTableColumnSpec> processColumns(WaterYearDefinition waterYearDefinition)
	{
		Map<Month, TreeTableColumnSpec> retval = new HashMap<>();
		getColumnSpecs().add(WATER_YEAR_COL_SPEC);
		Month startMonth = waterYearDefinition.getStartMonth();
		do
		{
			RmaTreeTableColumnSpec spec = new RmaTreeTableColumnSpec.Builder(startMonth.getDisplayName(TextStyle.SHORT, Locale.getDefault()))
					.withCanBeHidden(true)
					.withEditable(false)
					.withSortable(false)
					.withVisibleByDefault(true)
					.build();
			getColumnSpecs().add(spec);
			retval.put(startMonth, spec);
			startMonth = startMonth.plus(1);
		}
		while(startMonth != waterYearDefinition.getEndMonth().plus(1));
		return retval;
	}
}

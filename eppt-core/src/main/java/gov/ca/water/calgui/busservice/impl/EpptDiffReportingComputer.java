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

package gov.ca.water.calgui.busservice.impl;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.TreeMap;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.project.EpptScenarioRun;

import hec.heclib.util.HecTime;
import hec.io.TimeSeriesContainer;
import rma.util.RMAConst;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-26-2019
 */
class EpptDiffReportingComputer extends EpptReportingComputer
{

	private final EpptScenarioRun _baseRun;

	EpptDiffReportingComputer(EpptScenarioRun baseRun, GUILinksAllModelsBO guiLink, EpptStatistic statistics, MonthPeriod monthPeriod,
							  Map<EpptScenarioRun, WaterYearIndex> selectedIndicies, Map<EpptScenarioRun, List<WaterYearIndex>> allIndicies)
	{
		super(guiLink, statistics, monthPeriod, selectedIndicies, allIndicies);
		_baseRun = baseRun;
	}

	@Override
	NavigableMap<LocalDateTime, Double> getFullTimeSeries(LocalDate start, LocalDate end, int offset,
														  TimeSeriesContainer[] primarySeries)
	{
		NavigableMap<LocalDateTime, Double> baseTimeSeries = super.getFullTimeSeries(start, end, offset,
				buildDssGrabber(_baseRun, false, start, end).getPrimarySeries());
		NavigableMap<LocalDateTime, Double> fullSeries = new TreeMap<>();
		if(primarySeries != null && primarySeries.length > 0 && primarySeries[0] != null)
		{
			TimeSeriesContainer tsc = primarySeries[0];
			for(int i = 0; i < tsc.getNumberValues(); i++)
			{
				HecTime hecTime = tsc.getHecTime(i);
				double altValue = tsc.getValue(i);
				if(RMAConst.isValidValue(altValue) && altValue != -3.402823466E38)
				{
					Date javaDate = hecTime.getJavaDate(offset);
					LocalDateTime localDateTime = LocalDateTime.ofInstant(javaDate.toInstant(), ZoneId.systemDefault());
					Double baseValue = baseTimeSeries.getOrDefault(localDateTime, Double.NaN);
					if(RMAConst.isValidValue(baseValue) && baseValue != -3.402823466E38)
					{
						fullSeries.put(localDateTime, altValue - baseValue);
					}
				}
			}
			if(!fullSeries.isEmpty())
			{
				fullSeries = fullSeries.subMap(start.minusMonths(1).atTime(0, 0), true,
						end.plusMonths(1).atTime(0, 0), true);
			}
		}
		return fullSeries;
	}
}

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

package gov.ca.water.reportengine.standardsummary;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearPeriodRangesFilter;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssCache;
import gov.ca.water.reportengine.EpptReportException;

import rma.util.TwoColorColorContour;

import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-27-2019
 */
public class SummaryReportParameters
{
	private final WaterYearDefinition _waterYearDefinition;
	private final PercentDiffStyle _percentDiffStyle;
	private final TwoColorColorContour _positiveContour;
	private final TwoColorColorContour _negativeContour;
	private final List<String> _disabledSummaryModules;
	private final CommonPeriodFilter _commonPeriodFilter;
	private final DssCache _dssCache;
	private final List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> _waterYearPeriodRanges;

	public SummaryReportParameters(WaterYearDefinition waterYearDefinition,
								   List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> waterYearPeriodRanges,
								   PercentDiffStyle percentDiffStyle, List<String> disabledSummaryModules,
								   CommonPeriodFilter commonPeriodFilter, DssCache dssCache)
			throws EpptReportException
	{
		_waterYearDefinition = waterYearDefinition;
		_waterYearPeriodRanges = waterYearPeriodRanges;
		_percentDiffStyle = percentDiffStyle;
		_disabledSummaryModules = disabledSummaryModules;
		_commonPeriodFilter = commonPeriodFilter;
		_dssCache = dssCache;
		_positiveContour = createContour(Paths.get(Constant.POSITIVE_CONTOUR_FILE));
		_negativeContour = createContour(Paths.get(Constant.NEGATIVE_CONTOUR_FILE));
	}

	private TwoColorColorContour createContour(Path path) throws EpptReportException
	{

		TwoColorColorContour twoColorColorContour;
		try(BufferedReader reader = Files.newBufferedReader(path))
		{
			JAXBContext jc = JAXBContext.newInstance(TwoColorColorContour.class);
			Unmarshaller unmarshaller = jc.createUnmarshaller();
			twoColorColorContour = (TwoColorColorContour) unmarshaller.unmarshal(reader);
		}
		catch(IOException | JAXBException ex)
		{
			throw new EpptReportException("Unable to generate percent difference color contours", ex);
		}
		return twoColorColorContour;
	}

	public TwoColorColorContour getPositiveContour()
	{
		return copyContour(_positiveContour);
	}

	public TwoColorColorContour getNegativeContour()
	{
		return copyContour(_negativeContour);
	}

	private TwoColorColorContour copyContour(TwoColorColorContour twoColorColorContour)
	{
		TwoColorColorContour copy = new TwoColorColorContour();
		copy.setMinColor(twoColorColorContour.getMinColor());
		copy.setMaxColor(twoColorColorContour.getMaxColor());
		return copy;
	}

	public PercentDiffStyle getPercentDiffStyle()
	{
		return _percentDiffStyle;
	}

	public List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> getWaterYearPeriodRanges()
	{
		return _waterYearPeriodRanges;
	}

	public List<WaterYearPeriodRangeGroup> getGroupedWaterYearPeriodRanges()
	{
		List<WaterYearPeriodRangeGroup> retval = new ArrayList<>();
		List<String> groupNames = _waterYearPeriodRanges.stream()
														.map(Map::values)
														.filter(s -> !s.isEmpty())
														.map(s -> s.iterator().next())
														.map(WaterYearPeriodRangesFilter::getGroupName)
														.distinct()
														.collect(toList());
		for(String group : groupNames)
		{
			List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> filters = new ArrayList<>();
			for(Map<EpptScenarioRun, WaterYearPeriodRangesFilter> filterMap : _waterYearPeriodRanges)
			{
				if(!filterMap.isEmpty() && filterMap.values().iterator().next().getGroupName().equals(group))
				{
					filters.add(filterMap);
				}
			}
			retval.add(new WaterYearPeriodRangeGroup(group, filters));
		}
		return retval;
	}

	public WaterYearDefinition getWaterYearDefinition()
	{
		return _waterYearDefinition;
	}

	public List<String> getDisabledSummaryModules()
	{
		return _disabledSummaryModules;
	}

	public CommonPeriodFilter getCommonPeriodFilter()
	{
		return _commonPeriodFilter;
	}

	public DssCache getDssCache()
	{
		return _dssCache;
	}

	public static final class WaterYearPeriodRangeGroup
	{
		private final String _groupName;
		private final List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> _filters;

		private WaterYearPeriodRangeGroup(String groupName, List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> filters)
		{
			_groupName = groupName;
			_filters = filters;
		}

		public List<Map<EpptScenarioRun, WaterYearPeriodRangesFilter>> getFilters()
		{
			return _filters;
		}

		public String getGroupName()
		{
			return _groupName;
		}
	}
}

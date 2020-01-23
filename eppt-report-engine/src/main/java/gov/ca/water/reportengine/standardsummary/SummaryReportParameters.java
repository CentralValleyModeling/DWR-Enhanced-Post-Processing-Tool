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

package gov.ca.water.reportengine.standardsummary;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import gov.ca.water.calgui.bo.CommonPeriodFilter;
import gov.ca.water.calgui.bo.WaterYearDefinition;
import gov.ca.water.calgui.bo.WaterYearIndex;
import gov.ca.water.calgui.bo.WaterYearPeriod;
import gov.ca.water.calgui.bo.WaterYearPeriodRange;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import gov.ca.water.calgui.scripts.DssCache;
import gov.ca.water.reportengine.EpptReportException;

import rma.util.TwoColorColorContour;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-27-2019
 */
public class SummaryReportParameters
{
	private final WaterYearDefinition _waterYearDefinition;
	private final Map<EpptScenarioRun, WaterYearIndex> _waterYearIndex;
	private final WaterYearPeriodRange _longTermRange;
	private final Map<WaterYearPeriod, List<WaterYearPeriodRange>> _waterYearPeriodRanges;
	private final PercentDiffStyle _percentDiffStyle;
	private final TwoColorColorContour _positiveContour;
	private final TwoColorColorContour _negativeContour;
	private final List<String> _disabledSummaryModules;
	private final CommonPeriodFilter _commonPeriodFilter;
	private final DssCache _dssCache;

	public SummaryReportParameters(WaterYearDefinition waterYearDefinition, Map<EpptScenarioRun, WaterYearIndex> waterYearIndicies, WaterYearPeriodRange longTermRange,
								   Map<WaterYearPeriod, List<WaterYearPeriodRange>> waterYearPeriodRanges, PercentDiffStyle percentDiffStyle,
								   List<String> disabledSummaryModules, CommonPeriodFilter commonPeriodFilter, DssCache dssCache)
			throws EpptReportException
	{
		_waterYearDefinition = waterYearDefinition;
		_waterYearIndex = waterYearIndicies;
		_longTermRange = longTermRange;
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

	public Map<WaterYearPeriod, List<WaterYearPeriodRange>> getWaterYearPeriodRanges()
	{
		return _waterYearPeriodRanges;
	}

	public WaterYearDefinition getWaterYearDefinition()
	{
		return _waterYearDefinition;
	}

	public WaterYearIndex getWaterYearIndex(EpptScenarioRun epptScenarioRun)
	{
		return _waterYearIndex.get(epptScenarioRun);
	}

	public WaterYearPeriodRange getLongTermRange()
	{
		return _longTermRange;
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
}

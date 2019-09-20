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

package gov.ca.water.calgui.compute.stats;

import java.time.LocalDateTime;
import java.time.Month;
import java.util.List;
import java.util.Map;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;

import rma.stats.EmpiricalDist;

import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-30-2019
 */
public abstract class ExceedanceStatistic implements Statistics
{
	abstract double getCdfPercent();

	@Override
	public boolean isDisabled()
	{
		return false;
	}

	@Override
	public final Map<Month, Double> calculate(GUILinksAllModelsBO guiLink, Map<LocalDateTime, Double> input)
	{
		return input.entrySet().stream()
					.collect(groupingBy(e -> e.getKey().getMonth(), mapping(Map.Entry::getValue,
							collectingAndThen(toList(), this::toExceedance))));
	}

	private double calculateExceedance(double[] doubles)
	{
		EmpiricalDist empiricalDist = new EmpiricalDist(EmpiricalDist.InterpType.LINEAR, doubles);
		return empiricalDist.invCDF(getCdfPercent());
	}

	private Double toExceedance(List<Double> v)
	{
		return calculateExceedance(v.stream().mapToDouble(Double::doubleValue).toArray());
	}
}

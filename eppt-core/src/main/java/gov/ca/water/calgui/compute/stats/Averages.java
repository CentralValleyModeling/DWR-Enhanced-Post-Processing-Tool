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
import java.util.Map;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;

import rma.services.annotations.ServiceProvider;

import static java.util.stream.Collectors.averagingDouble;
import static java.util.stream.Collectors.groupingBy;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-24-2019
 */
@ServiceProvider(service = Statistics.class, position = 0)
public class Averages implements Statistics
{
	@Override
	public String getName()
	{
		return "Averages";
	}

	@Override
	public boolean isDisabled()
	{
		return false;
	}

	@Override
	public Map<Month, Double> calculate(GUILinksAllModelsBO guiLink, Map<LocalDateTime, Double> input)
	{
		return input.entrySet()
					.stream()
					.collect(groupingBy(e -> e.getKey().getMonth(), averagingDouble(Map.Entry::getValue)));

	}
}
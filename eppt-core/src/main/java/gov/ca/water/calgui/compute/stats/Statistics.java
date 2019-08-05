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
import java.util.HashMap;
import java.util.Map;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-23-2019
 */
public interface Statistics
{
	String getName();

	default Map<Month, Double> calculate(GUILinksAllModelsBO guiLink, Map<LocalDateTime, Double> input)
	{
		return new HashMap<>();
	}

	default boolean isDisabled()
	{
		return true;
	}
}

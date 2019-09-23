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

package gov.ca.water.trendreporting;

import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-23-2019
 */
public class TrendStatistics
{
	private final Path _jythonFilePath;

	public TrendStatistics(Path jythonFilePath)
	{
		_jythonFilePath = jythonFilePath;

	}

	public String getName()
	{
		return "";
	}

	public Map<Month, Double> calculate(Map<LocalDateTime, Double> filteredPeriod)
	{
		return new HashMap<>();
	}
}

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

package gov.ca.water.plotly;

import java.util.List;
import java.util.NavigableMap;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-19-2019
 */
public class ExceedanceData
{


	private final String _scenarioName;
	private final NavigableMap<Double, Double> _primaryData;
	private final List<NavigableMap<Double, Double>> _thresholdData;

	public ExceedanceData(String scenarioName, NavigableMap<Double, Double> primaryData, List<NavigableMap<Double, Double>> thresholdData)
	{
		_scenarioName = scenarioName;
		_primaryData = primaryData;
		_thresholdData = thresholdData;
	}

	public String getScenarioName()
	{
		return _scenarioName;
	}

	public NavigableMap<Double, Double> getPrimaryData()
	{
		return _primaryData;
	}

	public List<NavigableMap<Double, Double>> getThresholdData()
	{
		return _thresholdData;
	}
}

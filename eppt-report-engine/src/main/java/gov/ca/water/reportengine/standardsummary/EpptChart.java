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

import java.util.List;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class EpptChart
{
	private final String _module;
	private final String _section;
	private final String _subModule;
	private final ChartType _chartType;
	private final String _chartId;
	private final List<ChartComponent> _chartComponents;

	public EpptChart(String module, String section, String subModule, ChartType chartType, String chartId, List<ChartComponent> chartComponents)
	{
		_module = module.trim();
		_section = section.trim();
		_subModule = subModule.trim();
		_chartType = chartType;
		_chartId = chartId.trim();
		_chartComponents = chartComponents;
	}

	public ChartType getChartType()
	{
		return _chartType;
	}

	public String getModule()
	{
		return _module;
	}

	public String getSubModule()
	{
		return _subModule;
	}

	public String getSection()
	{
		return _section;
	}

	public String getChartId()
	{
		return _chartId;
	}

	public List<ChartComponent> getChartComponents()
	{
		return _chartComponents;
	}

	@Override
	public String toString()
	{
		return "Module: " + getModule() + ", Section: " + getSection() + ", Sub Module: " + getSubModule() + ", Chart Type: " + getChartType().getId() + ", Chart ID: " + getChartId();
	}
}

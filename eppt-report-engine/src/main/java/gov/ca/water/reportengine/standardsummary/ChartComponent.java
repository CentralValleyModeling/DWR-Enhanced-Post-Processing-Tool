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

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-26-2019
 */
public class ChartComponent
{
	private final String _title;
	private final String _header;
	private final String _subHeader;
	private final String _component;
	private final String _function;

	public ChartComponent(String title, String header, String subHeader, String component, String function)
	{
		_title = title.trim();
		_header = header.trim();
		_subHeader = subHeader.trim();
		_component = component.trim();
		_function = function.trim();
	}

	public String getTitle()
	{
		return _title;
	}

	public String getHeader()
	{
		return _header;
	}

	public String getSubHeader()
	{
		return _subHeader;
	}

	public String getComponent()
	{
		return _component;
	}

	public String getFunction()
	{
		return _function;
	}

	@Override
	public String toString()
	{
		return "Title: " + _title + ", Header: " + _header + ", Sub-Header: " + _subHeader + ", Component: " + _component + ", Function: " + _function;
	}
}

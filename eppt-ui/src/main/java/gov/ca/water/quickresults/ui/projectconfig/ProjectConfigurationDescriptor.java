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

package gov.ca.water.quickresults.ui.projectconfig;


/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-14-2019
 */
class ProjectConfigurationDescriptor
{
	private final String _name;
	private final String _description;

	ProjectConfigurationDescriptor(String name, String description)
	{
		_name = name;
		_description = description;
	}

	String getDescription()
	{
		return _description;
	}

	String getName()
	{
		return _name;
	}
}
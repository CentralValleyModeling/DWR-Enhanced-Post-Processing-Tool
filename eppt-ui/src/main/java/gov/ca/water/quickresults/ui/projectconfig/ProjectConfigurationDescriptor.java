/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

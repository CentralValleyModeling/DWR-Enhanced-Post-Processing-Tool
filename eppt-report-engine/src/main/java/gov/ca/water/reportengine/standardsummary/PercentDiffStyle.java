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

import org.antlr.runtime.TokenSource;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-28-2019
 */
public enum PercentDiffStyle
{
	ABSOLUTE("Absolute Only"), PERCENT("Percent Only"), FULL("Absolute and Percent");

	private final String _name;

	PercentDiffStyle(String name)
	{
		_name = name;
	}

	@Override
	public String toString()
	{
		return _name;
	}
}

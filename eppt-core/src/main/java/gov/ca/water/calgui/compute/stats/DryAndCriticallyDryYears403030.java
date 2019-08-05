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

import rma.services.annotations.ServiceProvider;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 07-24-2019
 */
@ServiceProvider(service=Statistics.class, position = 1500)
public class DryAndCriticallyDryYears403030 implements Statistics
{
	@Override
	public String getName()
	{
		return "Dry and Critically Dry Years (40-30-30, ELT)";
	}
}

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

package gov.ca.water.calgui.wresl;

import java.util.stream.IntStream;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 08-14-2019
 */
public class DssReader
{

	public IntStream getAltDts(int dtsLinkId)
	{
		return IntStream.of(100, 200, 100, 100, 100, 100);
	}

	public IntStream getBaseDts(int dtsLinkId)
	{
		return IntStream.of(100, 100, 100, 200, 300, 200);
	}
}

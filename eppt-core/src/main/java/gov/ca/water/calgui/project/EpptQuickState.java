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

package gov.ca.water.calgui.project;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-22-2019
 */
public final class EpptQuickState
{
	private static final Logger LOGGER = Logger.getLogger(EpptQuickState.class.getName());

	private EpptQuickState()
	{
		LOGGER.log(Level.FINEST, "Private ctor, must be created through Builder Pattern");
	}

	public static EpptQuickStateBuilder construct()
	{
		return new EpptQuickStateBuilder();
	}

	@Override
	public String toString()
	{
		return generateQuickState();
	}

	private String generateQuickState()
	{
		return null;
	}

	public static final class EpptQuickStateBuilder
	{
		private EpptQuickStateBuilder()
		{
			LOGGER.log(Level.FINEST, "Private ctor, must be created through Factory method");
		}

		public EpptQuickState build()
		{
			return new EpptQuickState();
		}
	}
}

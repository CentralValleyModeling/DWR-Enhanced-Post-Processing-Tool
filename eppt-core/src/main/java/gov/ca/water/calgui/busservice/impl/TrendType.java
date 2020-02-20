/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2020.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */

package gov.ca.water.calgui.busservice.impl;

import java.util.Objects;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 12-23-2019
 */
public class TrendType
{
	public static final String USER_DEFINED = "User Defined";
	public static final TrendType ALL_TREND_TYPE = new TrendType("All Types")
	{
		@Override
		public boolean matchesGuiLink(GUILinksAllModelsBO guiLink)
		{
			return true;
		}
	};
	public static final TrendType MISC_TREND_TYPE = new TrendType("Misc")
	{
		@Override
		public boolean matchesGuiLink(GUILinksAllModelsBO guiLink)
		{
			return guiLink != null && (guiLink.getType() == null || guiLink.getType().isEmpty());
		}
	};
	public static final TrendType USER_DEFINED_TREND_TYPE = new TrendType(USER_DEFINED)
	{
		@Override
		public boolean matchesGuiLink(GUILinksAllModelsBO guiLink)
		{
			return guiLink != null && (guiLink.getType().equalsIgnoreCase(USER_DEFINED));
		}
	};
	private final String _title;

	public TrendType(String title)
	{
		_title = title.trim();
	}

	public TrendType(GUILinksAllModelsBO guiLink)
	{
		_title = guiLink.getType();
	}

	public boolean matchesGuiLink(GUILinksAllModelsBO guiLink)
	{
		return guiLink != null && guiLink.getType().equalsIgnoreCase(_title);
	}

	public String getTitle()
	{
		return _title;
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
		{
			return true;
		}
		if(o == null || getClass() != o.getClass())
		{
			return false;
		}
		final TrendType trendType = (TrendType) o;
		return Objects.equals(getTitle().toLowerCase(), trendType.getTitle().toLowerCase());
	}

	@Override
	public int hashCode()
	{
		return Objects.hash(getTitle().toLowerCase());
	}

	@Override
	public String toString()
	{
		return getTitle();
	}
}

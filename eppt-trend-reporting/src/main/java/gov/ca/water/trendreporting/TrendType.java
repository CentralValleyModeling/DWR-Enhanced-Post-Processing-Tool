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

package gov.ca.water.trendreporting;

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
	static final TrendType ALL_TREND_TYPE = new TrendType("All Types")
	{
		@Override
		public boolean matchesGuiLink(GUILinksAllModelsBO guiLink)
		{
			return true;
		}
	};
	private final String _title;

	TrendType(String title)
	{
		_title = title.trim();
	}

	TrendType(GUILinksAllModelsBO guiLink)
	{
		String bAndCPart = guiLink.getPrimary()
								  .values()
								  .stream()
								  .filter(p -> !p.isEmpty())
								  .findAny()
								  .orElse("");
		String[] split = bAndCPart.split("/");
		if(split.length > 1)
		{
			_title = split[split.length - 1].trim();
		}
		else
		{
			_title = "";
		}
	}

	public boolean matchesGuiLink(GUILinksAllModelsBO guiLink)
	{
		boolean retval = false;
		if(guiLink != null)
		{
			if(_title.equalsIgnoreCase(guiLink.getPlotAxisLabel().trim()))
			{
				retval = true;
			}
			else
			{
				String bAndCPart = guiLink.getPrimary()
										  .values()
										  .stream()
										  .filter(p -> !p.isEmpty())
										  .findAny()
										  .orElse("");
				String[] split = bAndCPart.split("/");
				if(split.length > 1)
				{
					retval = _title.equalsIgnoreCase(split[split.length - 1].trim());
				}
			}
		}
		return retval;
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

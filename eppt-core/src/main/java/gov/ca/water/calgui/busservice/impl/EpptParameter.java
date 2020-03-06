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

import java.util.Optional;

import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.IGuiLinksSeedDataSvc;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 02-20-2020
 */
public class EpptParameter
{
	private final int _index;
	private GUILinksAllModelsBO _guiLink;
	private String _titleOverride;
	private final boolean _editable;

	EpptParameter(int index, Integer guiLinkId, String titleOverride)
	{
		_editable = false;
		_index = index;
		IGuiLinksSeedDataSvc guiLinkService = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance();
		if(guiLinkId != null)
		{
			_guiLink = guiLinkService.getGuiLink(guiLinkId.toString());
		}
		else
		{
			_guiLink = null;
		}
		_titleOverride = titleOverride;
	}

	private EpptParameter(int index, GUILinksAllModelsBO guiLink)
	{
		_editable = true;
		_index = index;
		_guiLink = guiLink;
		_titleOverride = guiLink.getPlotTitle();
	}

	public static EpptParameter create(int index, GUILinksAllModelsBO guiLink)
	{
		return new EpptParameter(index, guiLink);
	}

	public GUILinksAllModelsBO getGuiLink()
	{
		return _guiLink;
	}

	int getIndex()
	{
		return _index;
	}

	public boolean isEditable()
	{
		return _editable;
	}

	@Override
	public String toString()
	{
		String title = _titleOverride;
		GUILinksAllModelsBO guiLink = getGuiLink();
		if(guiLink != null)
		{
			if(title == null || title.isEmpty())
			{
				title = guiLink.getPlotTitle();
			}
			if(title == null || title.isEmpty())
			{
				title = guiLink.getPrimary()
							   .values()
							   .stream()
							   .filter(p -> !p.isEmpty())
							   .map(p->p.split("/")[0])
							   .findAny()
							   .orElse("");
			}
		}
		return Optional.ofNullable(title).orElse("");
	}

	public void update(GUILinksAllModelsBO guiLink)
	{
		if(_editable)
		{
			_guiLink = guiLink;
			_titleOverride = guiLink.getPlotTitle();
		}
	}
}

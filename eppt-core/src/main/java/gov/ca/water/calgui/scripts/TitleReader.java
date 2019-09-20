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

package gov.ca.water.calgui.scripts;

import java.util.ArrayList;
import java.util.List;

import gov.ca.water.calgui.EpptInitializationException;
import gov.ca.water.calgui.bo.DetailedIssue;
import gov.ca.water.calgui.bo.GUILinksAllModelsBO;
import gov.ca.water.calgui.busservice.impl.DetailedIssuesReader;
import gov.ca.water.calgui.busservice.impl.GuiLinksSeedDataSvcImpl;
import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.constant.EpptPreferences;
import gov.ca.water.calgui.project.EpptScenarioRun;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 09-06-2019
 */
public class TitleReader
{
	private final EpptScenarioRun _scenarioRun;

	public TitleReader(EpptScenarioRun scenarioRun)
	{
		_scenarioRun = scenarioRun;
	}

	public String getDtsTitle(int detailsLink)
	{
		return DetailedIssuesReader.getInstance().getDetailedIssues().stream()
								   .filter(di -> di.getDetailedIssueId() == detailsLink)
								   .map(detailedIssue ->
								   {
									   String title = detailedIssue.getTitle();
									   if(title == null || title.isEmpty())
									   {
										   title = detailedIssue.getLinkedVar();
									   }
									   return title;
								   })
								   .findAny()
								   .orElse("");
	}

	public String getGuiLinkId(int i)
	{
		GUILinksAllModelsBO objById = GuiLinksSeedDataSvcImpl.getSeedDataSvcImplInstance().getObjById(String.valueOf(i));
		if(objById != null)
		{
			String plotTitle = objById.getPlotTitle();
			if(plotTitle == null || plotTitle.isEmpty())
			{
				if(_scenarioRun != null)
				{
					plotTitle = objById.getPrimary().get(_scenarioRun.getModel());
				}
				else
				{
					plotTitle = objById.getPrimary().values().stream().findAny().orElse("");
				}
			}
			return plotTitle;
		}
		else
		{
			return "";
		}
	}
}

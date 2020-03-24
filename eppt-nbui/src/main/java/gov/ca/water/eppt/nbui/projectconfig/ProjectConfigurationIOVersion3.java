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

package gov.ca.water.eppt.nbui.projectconfig;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.json.JSONObject;

import static gov.ca.water.eppt.nbui.projectconfig.ProjectConfigurationIO.SCENARIO_LOOKUP_DIR;
import static gov.ca.water.eppt.nbui.projectconfig.ProjectConfigurationIO.SCENARIO_WRESL_DIR;
import static gov.ca.water.eppt.nbui.projectconfig.ProjectConfigurationIO.VERSION_3_0;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 05-06-2019
 */
class ProjectConfigurationIOVersion3 extends ProjectConfigurationIOVersion2
{

	String getVersion()
	{
		return VERSION_3_0;
	}

	@Override
	Path readWreslDirectory(JSONObject scenarioJson)
	{
		Path wreslMain = Paths.get("");
		if(scenarioJson.has(SCENARIO_WRESL_DIR))
		{
			wreslMain = unrelativizeFromInstaller(scenarioJson.getString(SCENARIO_WRESL_DIR));
		}
		return wreslMain;
	}

	@Override
	Path readLookupDirectory(JSONObject scenarioJson)
	{
		Path waterYearTable = Paths.get("");
		if(scenarioJson.has(SCENARIO_LOOKUP_DIR))
		{
			waterYearTable = unrelativizeFromInstaller(scenarioJson.getString(SCENARIO_LOOKUP_DIR));
		}
		return waterYearTable;
	}
}

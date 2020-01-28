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

import gov.ca.water.calgui.constant.Constant;
import gov.ca.water.calgui.project.EpptScenarioRun;
import javafx.scene.paint.Color;
import org.json.JSONObject;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 01-24-2020
 */
public class EpptReportingScenarioComputed
{
	private static final String SCENARIO_NAME_KEY = "scenario_name";
	private static final String PRIMARY_DATA_KEY = "primary_data";
	private static final String SECONDARY_DATA_KEY = "secondary_data";
	private static final String SCENARIO_COLOR = "scenario_color";
	private final EpptScenarioRun _epptScenarioRun;
	private final EpptReportingComputed _primary;
	private final EpptReportingComputed _secondary;

	EpptReportingScenarioComputed(EpptScenarioRun epptScenarioRun, EpptReportingComputed primary, EpptReportingComputed secondary)
	{
		_epptScenarioRun = epptScenarioRun;
		_primary = primary;
		_secondary = secondary;
	}

	public EpptScenarioRun getEpptScenarioRun()
	{
		return _epptScenarioRun;
	}

	public EpptReportingComputed getPrimary()
	{
		return _primary;
	}

	public EpptReportingComputed getSecondary()
	{
		return _secondary;
	}

	JSONObject toJson()
	{
		JSONObject retval = new JSONObject();
		retval.put(SCENARIO_NAME_KEY, _epptScenarioRun.getName());
		retval.put(PRIMARY_DATA_KEY, _primary.toJson());
		retval.put(SECONDARY_DATA_KEY, _secondary.toJson());
		retval.put(SCENARIO_COLOR, Constant.colorToHex(_epptScenarioRun.getColor()));
		return retval;
	}
}

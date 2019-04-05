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

import java.util.Collections;
import java.util.List;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-22-2019
 */
public class EpptProject
{
	private final List<EpptScenarioRun> _scenarioRuns;
	private String _description;
	private String _name;

	public EpptProject(String name, String description, List<EpptScenarioRun> scenarioRuns)
	{
		_name = name;
		_description = description;
		_scenarioRuns = Collections.unmodifiableList(scenarioRuns);
	}


	/**
	 * @return name of Project configuration
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * @return immutable list of Senario runs
	 */
	public List<EpptScenarioRun> getScenarioRuns()
	{
		return _scenarioRuns;
	}

	/**
	 * @return description of Project configuration
	 */
	public String getDescription()
	{
		return _description;
	}
}

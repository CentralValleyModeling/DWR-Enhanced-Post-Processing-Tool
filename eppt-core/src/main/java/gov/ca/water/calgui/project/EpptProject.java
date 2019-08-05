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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jfree.data.time.Month;

/**
 * Company: Resource Management Associates
 *
 * @author <a href="mailto:adam@rmanet.com">Adam Korynta</a>
 * @since 03-22-2019
 */
public class EpptProject
{
	private final List<EpptScenarioRun> _scenarioRuns;
	private final LocalDate _startMonth;
	private final LocalDate _endMonth;
	private final Map<String, Boolean> _selectedComponents;
	private String _description;
	private String _name;

	public EpptProject(String name, String description, List<EpptScenarioRun> scenarioRuns, LocalDate startMonth,
					   LocalDate endMonth, Map<String, Boolean> selectedComponents)
	{
		_name = name;
		_description = description;
		_scenarioRuns = scenarioRuns;
		_startMonth = startMonth;
		_endMonth = endMonth;
		_selectedComponents = new HashMap<>(selectedComponents);
	}

	public LocalDate getStartMonth()
	{
		return _startMonth;
	}

	public LocalDate getEndMonth()
	{
		return _endMonth;
	}

	public Map<String, Boolean> getSelectedComponents()
	{
		return _selectedComponents;
	}

	/**
	 * @return name of Project configuration
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 * @return copy list of Senario runs
	 */
	public List<EpptScenarioRun> getScenarioRuns()
	{
		return new ArrayList<>(_scenarioRuns);
	}

	public void addScenarioRun(EpptScenarioRun scenarioRun)
	{
		_scenarioRuns.add(scenarioRun);
	}

	/**
	 * @return description of Project configuration
	 */
	public String getDescription()
	{
		return _description;
	}
}

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

package gov.ca.water.bo;
//! Supporting class for scenario comparisons

/**
 * This will hold the intermediate information used for building the Scenario
 * Display.
 *
 * @author Mohan
 */
public class ScenarioDisplayBO
{
	/**
	 * This field will hold the text of the component to display it in
	 * "View Scenario Settings".
	 */
	private String _componentText;
	/**
	 * This field will hold the parent text of the component to display it in
	 * "View Scenario Settings".
	 */
	private String _componentParents;
	/**
	 * This field will hold the value of the component to display it in
	 * "View Scenario Settings". ex Default.cls, Default_dv.dss.
	 */
	private String _componentValue;

	public ScenarioDisplayBO(String componentText, String componentParents, String componentValue)
	{
		_componentText = componentText;
		_componentParents = componentParents;
		_componentValue = componentValue;
	}

	public String getComponentText()
	{
		return _componentText;
	}

	public void setComponentText(String componentText)
	{
		_componentText = componentText;
	}

	public String getComponentParents()
	{
		return _componentParents;
	}

	public void setComponentParents(String componentParents)
	{
		_componentParents = componentParents;
	}

	public String getComponentValue()
	{
		return _componentValue;
	}

	public void setComponentValue(String componentValue)
	{
		_componentValue = componentValue;
	}
}

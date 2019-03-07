/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
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

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
	String componentText;
	/**
	 * This field will hold the parent text of the component to display it in
	 * "View Scenario Settings".
	 */
	String componentParents;
	/**
	 * This field will hold the value of the component to display it in
	 * "View Scenario Settings". ex Default.cls, Default_dv.dss.
	 */
	String componentValue;

	public ScenarioDisplayBO(String componentText, String componentParents, String componentValue)
	{
		this.componentText = componentText;
		this.componentParents = componentParents;
		this.componentValue = componentValue;
	}

	public String getComponentText()
	{
		return componentText;
	}

	public void setComponentText(String componentText)
	{
		this.componentText = componentText;
	}

	public String getComponentParents()
	{
		return componentParents;
	}

	public void setComponentParents(String componentParents)
	{
		this.componentParents = componentParents;
	}

	public String getComponentValue()
	{
		return componentValue;
	}

	public void setComponentValue(String componentValue)
	{
		this.componentValue = componentValue;
	}
}

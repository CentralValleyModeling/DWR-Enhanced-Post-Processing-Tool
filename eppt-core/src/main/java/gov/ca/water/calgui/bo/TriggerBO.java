/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;

//! Representation of dynamic behavior control record

/**
 * This is used to hold the information of Trigger*.table in memory.
 *
 * @author Mohan
 */
public class TriggerBO
{
	/**
	 * The trigger gui id is the one which says the id for the Dynamic action.
	 */
	private String triggerGuiId;
	/**
	 * The trigger action is the one which says whether it's selected or
	 * not-selected for the trigger gui id.
	 */
	private String triggerAction;
	/**
	 * The affectde gui id is the one which says the id for which the Dynamic
	 * action is going to apply.
	 */
	private String affectdeGuiId;
	/**
	 * The affected action is the one which says what should happen for the
	 * affected gui id.
	 */
	private String affectdeAction;

	public TriggerBO(String triggerGuiId, String triggerAction, String affectdeGuiId, String affectdeAction)
	{
		this.triggerGuiId = triggerGuiId;
		this.triggerAction = triggerAction;
		this.affectdeGuiId = affectdeGuiId;
		this.affectdeAction = affectdeAction;
	}

	public String getTriggerGuiId()
	{
		return triggerGuiId;
	}

	public void setTriggerGuiId(String triggerGuiId)
	{
		this.triggerGuiId = triggerGuiId;
	}

	public String getTriggerAction()
	{
		return triggerAction;
	}

	public void setTriggerAction(String triggerAction)
	{
		this.triggerAction = triggerAction;
	}

	public String getAffectdeGuiId()
	{
		return affectdeGuiId;
	}

	public void setAffectdeGuiId(String affectdeGuiId)
	{
		this.affectdeGuiId = affectdeGuiId;
	}

	public String getAffectdeAction()
	{
		return affectdeAction;
	}

	public void setAffectdeAction(String affectdeAction)
	{
		this.affectdeAction = affectdeAction;
	}
}
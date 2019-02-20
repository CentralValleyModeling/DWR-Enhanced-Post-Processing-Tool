/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.bo;
//! Monitors changes to inputs in the GUI

/**
 * This is holding the information for Audit.
 *
 * @author Mohan
 */
public class AuditBO
{
	/**
	 * The control Id of the record.
	 */
	private String controlId;
	/**
	 * The old value of the control id.
	 */
	private String oldValue;
	/**
	 * The new value of the control id.
	 */
	private String newValue;

	public AuditBO(String controlId, String oldValue, String newValue)
	{
		super();
		this.controlId = controlId;
		this.oldValue = oldValue;
		this.newValue = newValue;
	}

	public String getControlId()
	{
		return controlId;
	}

	public void setControlId(String controlId)
	{
		this.controlId = controlId;
	}

	public String getOldValue()
	{
		return oldValue;
	}

	public void setOldValue(String oldValue)
	{
		this.oldValue = oldValue;
	}

	public String getNewValue()
	{
		return newValue;
	}

	public void setNewValue(String newValue)
	{
		this.newValue = newValue;
	}
}

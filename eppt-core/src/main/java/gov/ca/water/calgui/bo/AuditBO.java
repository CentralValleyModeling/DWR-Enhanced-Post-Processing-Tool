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
	private String _controlId;
	/**
	 * The old value of the control id.
	 */
	private String _oldValue;
	/**
	 * The new value of the control id.
	 */
	private String _newValue;

	public AuditBO(String controlId, String oldValue, String newValue)
	{
		super();
		this._controlId = controlId;
		this._oldValue = oldValue;
		this._newValue = newValue;
	}

	public String getControlId()
	{
		return _controlId;
	}

	public void setControlId(String controlId)
	{
		this._controlId = controlId;
	}

	public String getOldValue()
	{
		return _oldValue;
	}

	public void setOldValue(String oldValue)
	{
		this._oldValue = oldValue;
	}

	public String getNewValue()
	{
		return _newValue;
	}

	public void setNewValue(String newValue)
	{
		this._newValue = newValue;
	}
}

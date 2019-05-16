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

package gov.ca.water.calgui.techservice;

/**
 * This is the interface for processing all the Audit records like adding,
 * delete etc.
 *
 * @author Mohan
 */
public interface IAuditSvc
{
	/**
	 * This will add the value to the Audit record.
	 *
	 * @param controlId The id of the control.
	 * @param oldValue  The old value of the control.
	 * @param newValue  The new value of the control.
	 */
	void addAudit(String controlId, String oldValue, String newValue);

	/**
	 * This will remove all the Audit records.
	 */
	void clearAudit();

	/**
	 * This will tell whether the Audit is empty or not.
	 *
	 * @return Will return false if the audit is empty and vise versa.
	 */
	boolean hasValues();
}

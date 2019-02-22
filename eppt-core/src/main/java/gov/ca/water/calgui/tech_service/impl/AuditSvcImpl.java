/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package gov.ca.water.calgui.tech_service.impl;

import java.util.ArrayList;
import java.util.List;

import gov.ca.water.calgui.bo.AuditBO;
import gov.ca.water.calgui.tech_service.IAuditSvc;

/**
 * This is the class for processing all the Audit records like adding, delete
 * etc.
 *
 * @author Mohan
 */
public final class AuditSvcImpl implements IAuditSvc
{
	private static final IAuditSvc AUDIT_SVC = new AuditSvcImpl();
	private final List<AuditBO> _auditRecord;

	private AuditSvcImpl()
	{
		_auditRecord = new ArrayList<>();
	}

	/**
	 * This method is for implementing the singleton. It will return the
	 * instance of this class if it is empty it will create one.
	 *
	 * @return Will return the instance of this class if it is empty it will
	 * create one.
	 */
	public static IAuditSvc getAuditSvcImplInstance()
	{
		return AUDIT_SVC;
	}

	@Override
	public void addAudit(String controlId, String oldValue, String newValue)
	{
		_auditRecord.add(new AuditBO(controlId, oldValue, newValue));
	}

	@Override
	public void clearAudit()
	{
		_auditRecord.clear();
	}

	@Override
	public boolean hasValues()
	{
		return !_auditRecord.isEmpty();
	}
}

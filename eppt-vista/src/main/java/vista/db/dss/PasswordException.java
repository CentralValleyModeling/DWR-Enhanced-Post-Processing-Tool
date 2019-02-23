/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.db.dss;

import java.rmi.RemoteException;

/**
 * 
 * 
 * @author Nicky Sandhu
 * @version $Id: PasswordException.java,v 1.1 2003/10/02 20:48:46 redwood Exp $
 */
public class PasswordException extends RemoteException {
	public PasswordException(String msg) {
		super(msg);
	}
}

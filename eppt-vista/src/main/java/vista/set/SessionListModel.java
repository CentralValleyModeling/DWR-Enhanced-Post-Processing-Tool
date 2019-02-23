/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

import javax.swing.*;

/**
 * a model to list groups in a session.
 */
public class SessionListModel extends AbstractListModel {
	/**
	 * A list model for this sesion
	 */
	public SessionListModel(Session s) {
		_session = s;
	}

	/**
	 * return the session for this model
	 */
	public Session getSession() {
		return _session;
	}

	/**
	 * gets element at particular index
	 */
	public Object getElementAt(int index) {
		return _session.getGroup(index).toString();
	}

	/**
	 * gets the size of the list
	 */
	public int getSize() {
		if (_session != null)
			return _session.getNumberOfGroups();
		else
			return 0;
	}

	/**
	 * this session
	 */
	private Session _session;
}

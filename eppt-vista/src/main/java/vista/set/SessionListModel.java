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

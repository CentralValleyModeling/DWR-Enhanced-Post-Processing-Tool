/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * A proxy for this session which loads the groups only when operations other
 * than setName() and getName() are requested.
 * 
 * @author Nicky Sandhu
 * @version $Id: SessionProxy.java,v 1.1 2003/10/02 20:49:31 redwood Exp $
 */
public abstract class SessionProxy extends Session {
	/**
	 * return the initialized session object. This is the session that contains
	 * the state which is then copied into the proxy objects state to initialize
	 * it.
	 */
	protected abstract Session getInitializedSession();

	/**
	 * copies the groups from this session into specified one.
	 */
	public void copyInto(Session s) {
		if (!_initialized) {
			initializeSession();
		}
		super.copyInto(s);
	}

	/**
	 * a shallow copy of the name and list of groups
	 */
	public Object clone() {
		if (!_initialized) {
			initializeSession();
		}
		return super.clone();
	}

	/**
	 * Creates a union of this session with given session. This is basically
	 * combining the group(s) in both sessions in one session and returning it.
	 */
	public Session createUnion(Session s) {
		if (!_initialized) {
			initializeSession();
		}
		return super.createUnion(s);
	}

	/**
	 * gets the number of groups in the list.
	 */
	public int getNumberOfGroups() {
		if (!_initialized) {
			initializeSession();
		}
		return super.getNumberOfGroups();
	}

	/**
	 * adds group if not already present
	 */
	public void addGroup(Group g) {
		if (!_initialized) {
			initializeSession();
		}
		super.addGroup(g);
	}

	/**
	 * removes group from list.
	 */
	public void removeGroup(Group g) {
		if (!_initialized) {
			initializeSession();
		}
		super.removeGroup(g);
	}

	/**
	 * gets the group by index
	 */
	public Group getGroup(int index) {
		if (!_initialized) {
			initializeSession();
		}
		return super.getGroup(index);
	}

	/**
	 * gets group by name
	 */
	public Group getGroup(String groupName) {
		if (!_initialized) {
			initializeSession();
		}
		return super.getGroup(groupName);
	}

	/**
	 * gets all the groups
	 */
	public Group[] getAllGroups() {
		if (!_initialized) {
			initializeSession();
		}
		return super.getAllGroups();
	}

	/**
	 * adds state listener
	 */
	void addStateListener(SessionProxyState s) {
		_stateListener = s;
	}

	/**
	 * informs state listener that session has been initialized
	 */
	void informStateListener() {
		if (_stateListener != null) {
			_stateListener.sessionIsInitialized();
		}
	}

	/**
	 * initialize session and set initialize to true.
	 */
	private void initializeSession() {
		Session s = getInitializedSession();
		s.copyInto(this);
		_initialized = true;
		informStateListener();
	}

	/**
	 * true if initialized
	 */
	private boolean _initialized = false;
	/**
	 * state object listening for initialization on this proxy.
	 */
	private SessionProxyState _stateListener;
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * The state pattern for use on proxy so that once initialized there is no
 * overhead for checking for initialization. It does this by delegating to proxy
 * object if uninitialized and to the actual object once initialized...
 * 
 * @author Nicky Sandhu
 * @version $Id: SessionProxyState.java,v 1.1 2003/10/02 20:49:32 redwood Exp $
 */
public abstract class SessionProxyState extends Session {
	/**
	 * initializes state object with given proxy. The proxy informs this state
	 * object on being initialized.
	 */
	public SessionProxyState(SessionProxy proxy) {
		proxy.addStateListener(this);
		_delegate = proxy;
	}

	/**
	 * copies the groups from this session into specified one.
	 */
	public void copyInto(Session s) {
		_delegate.copyInto(s);
	}

	/**
	 * a shallow copy of the name and list of groups
	 */
	public Object clone() {
		return _delegate.clone();
	}

	/**
	 * Creates a union of this session with given session. This is basically
	 * combining the group(s) in both sessions in one session and returning it.
	 */
	public Session createUnion(Session s) {
		return _delegate.createUnion(s);
	}

	/**
	 * gets the number of groups in the list.
	 */
	public int getNumberOfGroups() {
		return _delegate.getNumberOfGroups();
	}

	/**
	 * adds group if not already present
	 */
	public void addGroup(Group g) {
		_delegate.addGroup(g);
	}

	/**
	 * removes group from list.
	 */
	public void removeGroup(Group g) {
		_delegate.removeGroup(g);
	}

	/**
	 * gets the group by index
	 */
	public Group getGroup(int index) {
		return _delegate.getGroup(index);
	}

	/**
	 * gets group by name
	 */
	public Group getGroup(String groupName) {
		return _delegate.getGroup(groupName);
	}

	/**
	 * gets all the groups
	 */
	public Group[] getAllGroups() {
		return _delegate.getAllGroups();
	}

	/**
	 * initialize session and set initialize to true.
	 */
	void sessionIsInitialized() {
		Session s = new Session();
		_delegate.copyInto(s);
		_delegate = s;
	}

	/**
	 * the session which delegates to the proxy or actual session depending upon
	 * the state of the proxy.
	 */
	private Session _delegate;
}

/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app;

import vista.set.Group;
import vista.set.Session;

/**
 * A context within which a session lies.
 * 
 * @author Nicky Sandhu
 * @version $Id: SessionContext.java,v 1.1 2003/10/02 20:48:40 redwood Exp $
 */
public class SessionContext {
	private Session _currentSession;
	private Group _currentGroup;

	/**
	 * main application constructor
	 */
	public SessionContext(Session initialSession) {
		setCurrentSession(initialSession);
	}

	/**
	 * sets current session to given session
	 */
	public void setCurrentSession(Session s) {
		_currentSession = s;
	}

	/**
	 * sets current group to given group
	 */
	public void setCurrentGroup(Group g) {
		_currentGroup = g;
	}

	/**
	 * gets current session to given session
	 */
	public Session getCurrentSession() {
		return _currentSession;
	}

	/**
	 * gets current group to given group
	 */
	public Group getCurrentGroup() {
		return _currentGroup;
	}
	/**
	 * public SessionView getCurrentSessionView(){ return _sessionView; } public
	 * GroupView getCurrentGroupView(){ return _groupView; }
	 */
}

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
package vista.app;

import vista.set.Group;
import vista.set.Session;

/**
 * A context within which a session lies.
 *
 * @author Nicky Sandhu
 * @version $Id: SessionContext.java,v 1.1 2003/10/02 20:48:40 redwood Exp $
 */
public class SessionContext
{
	private Session _currentSession;
	private Group _currentGroup;

	/**
	 * main application constructor
	 */
	public SessionContext(Session initialSession)
	{
		setCurrentSession(initialSession);
	}

	/**
	 * gets current session to given session
	 */
	public Session getCurrentSession()
	{
		return _currentSession;
	}

	/**
	 * sets current session to given session
	 */
	public void setCurrentSession(Session s)
	{
		_currentSession = s;
	}

	/**
	 * gets current group to given group
	 */
	public Group getCurrentGroup()
	{
		return _currentGroup;
	}

	/**
	 * sets current group to given group
	 */
	public void setCurrentGroup(Group g)
	{
		_currentGroup = g;
	}
	/**
	 * public SessionView getCurrentSessionView(){ return _sessionView; } public
	 * GroupView getCurrentGroupView(){ return _groupView; }
	 */
}

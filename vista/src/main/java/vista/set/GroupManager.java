/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.set;

/**
 * Manages groups.
 *
 * @author Nicky Sandhu
 * @version $Id: GroupManager.java,v 1.1 2003/10/02 20:49:24 redwood Exp $
 */
public interface GroupManager extends Named
{
	/**
	 * The number of groups
	 */
	int getNumberOfGroups();

	/**
	 * Adds group
	 */
	void addGroup(Group group);

	/**
	 * removes group
	 */
	void removeGroup(Group group);

	/**
	 * gets group by index
	 */
	Group getGroup(int index);

	/**
	 * gets group by name
	 */
	Group getGroup(String groupName);

	/**
	 * gets all the groups
	 */
	Group[] getAllGroups();
}

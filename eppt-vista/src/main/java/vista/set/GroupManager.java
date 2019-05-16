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

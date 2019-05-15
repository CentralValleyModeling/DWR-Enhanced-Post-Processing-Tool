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

import javax.swing.*;

import vista.set.Group;

/**
 * Displays group as a list with pathname as a string "/A/B/C/D/E/F".
 * 
 * @author Nicky Sandhu
 * @version $Id: GroupList.java,v 1.2 1999/01/07 21:05:32 nsandhu Exp $
 */
public class GroupList extends AbstractListModel {
	/**
	 * Generates list for group
	 */
	public GroupList(Group g) {
		_group = g;
		_list = new JList(this);
		_list.setVisible(true);
	}

	/**
	 * gets element at particular index
	 */
	public Object getElementAt(int index) {
		return _group.getDataReference(index);
		// .getPathname();
	}

	/**
	 * gets the size of the list
	 */
	public int getSize() {
		return _group.getNumberOfDataReferences();
	}

	/**
	 * gets the completed list
	 */
	public JList getListBox() {
		return _list;
	}

	/**
   *
   */
	Group getGroup() {
		return _group;
	}

	/**
   *
   */
	void setGroup(Group g) {
		_group = g;
	}

	/**
	 * the list
	 */
	private JList _list;
	/**
	 * the group
	 */
	private Group _group;
}

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

package calsim.gym;

/**
 * Represents a node in the system. A node
 * is a junction into which arcs abut.
 *
 * @author Nicky Sandhu
 */
public class StorageSubNode extends Node
{
	private int _level;

	/**
	 * a node with a given id
	 */
	public StorageSubNode(int id, int level)
	{
		super(id);
		_level = level;
	}

	/**
	 * gets name
	 */
	public String getName()
	{
		return "NODE: " + getName() + "_" + getLevel();
	}

	/**
	 * gets the integer id for this node.
	 */
	public int getLevel()
	{
		return _level;
	}
}

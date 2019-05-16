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
 * Represents a point node in the system. Such a node is usually invisible in
 * a schematic and is to avoid so called hanging arcs.
 *
 * @author Nicky Sandhu
 */
public class PointNode extends Node
{
	private String _name;

	/**
	 * a node with a given id
	 */
	public PointNode(String nm)
	{
		super(-1);
		_name = nm;
	}

	/**
	 * gets name
	 */
	public String getName()
	{
		return _name;
	}
}

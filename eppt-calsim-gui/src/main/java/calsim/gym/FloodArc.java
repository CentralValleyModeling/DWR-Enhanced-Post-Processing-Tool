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
 * A Flood arc
 *
 * @author Nicky Sandhu
 */
public class FloodArc extends Arc
{
	/**
	 * construct a flood arc between the two nodes
	 */
	public FloodArc(String name, Node upNode, Node downNode)
	{
		super(name, upNode, downNode);
		setPriority(GymUtils.MIN_PRIORITY);
	}

	/**
	 * type abbrev impl.
	 */
	public String getTypeAbbrev()
	{
		return "F";
	}

	/**
	 * type name impl
	 */
	public String getTypeName()
	{
		return "FLOOD-ARC";
	}
}

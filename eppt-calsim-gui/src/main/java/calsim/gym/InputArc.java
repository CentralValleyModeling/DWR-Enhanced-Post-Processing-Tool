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
 * An input arc
 *
 * @author Nicky Sandhu
 */
public class InputArc extends Arc
{
	/**
	 *
	 */
	public InputArc(String name, Node upNode, Node downNode)
	{
		super(name, upNode, downNode);
	}

	/**
	 * type abbrev impl.
	 */
	public String getTypeAbbrev()
	{
		return "I";
	}

	/**
	 * type name impl
	 */
	public String getTypeName()
	{
		return "INPUT-ARC";
	}
}

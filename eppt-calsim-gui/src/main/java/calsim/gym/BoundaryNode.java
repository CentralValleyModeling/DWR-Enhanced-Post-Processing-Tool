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
 * A boundary node. Usually only there is but
 * one boundary node for the entire system.
 *
 * @author Nicky Sandhu
 */
public class BoundaryNode extends Node
{
	/**
	 * construct a boundary node with given id.
	 */
	public BoundaryNode(int id)
	{
		super(new Integer(id).toString());
	}
}

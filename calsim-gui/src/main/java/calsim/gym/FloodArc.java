/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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

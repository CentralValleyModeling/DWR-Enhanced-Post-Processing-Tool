/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gym;

/**
 * A channel arc
 *
 * @author Nicky Sandhu
 */
public class ChannelArc extends Arc
{
	/**
	 *
	 */
	public ChannelArc(String name, Node upNode, Node downNode)
	{
		super(name, upNode, downNode);
	}

	/**
	 * type abbrev impl.
	 */
	public String getTypeAbbrev()
	{
		return "C";
	}

	/**
	 * type name impl
	 */
	public String getTypeName()
	{
		return "CHANNEL-ARC";
	}
}

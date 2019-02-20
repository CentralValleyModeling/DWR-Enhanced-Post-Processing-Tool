/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gym;

/**
 * A demand arc
 *
 * @author Nicky Sandhu
 */
public class DemandArc extends Arc
{
	/**
	 *
	 */
	public DemandArc(String name, Node upNode, Node downNode)
	{
		super(name, upNode, downNode);
	}

	/**
	 * type abbrev impl.
	 */
	public String getTypeAbbrev()
	{
		return "D";
	}

	/**
	 * type name impl
	 */
	public String getTypeName()
	{
		return "DEMAND-ARC";
	}
}

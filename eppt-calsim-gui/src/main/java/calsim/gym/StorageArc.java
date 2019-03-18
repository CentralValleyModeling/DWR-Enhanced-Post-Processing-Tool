/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */

package calsim.gym;

/**
 * A Storage arc
 * It is given a priority of GymUtils.MAX_PRIORITY if it is a dead storage
 * or it is given a priority of
 *
 * @author Nicky Sandhu
 */
public class StorageArc extends Arc
{
	/**
	 * construct a storage arc between the two nodes and
	 * set priority to GymUtils.MAX_PRIORITY
	 */
	public StorageArc(String name, Node upNode, Node downNode)
	{
		super(name, upNode, downNode);
		if(GymUtils.isDeadStorageArc(name))
		{
			setPriority(1); // highest priority
		}
		else if(GymUtils.isFloodStorageArc(name))
		{
			setPriority(GymUtils.MIN_PRIORITY / 10);
		}
	}

	/**
	 * type abbrev impl.
	 */
	public String getTypeAbbrev()
	{
		return "S";
	}

	/**
	 * type name impl
	 */
	public String getTypeName()
	{
		return "STORAGE-ARC";
	}
}

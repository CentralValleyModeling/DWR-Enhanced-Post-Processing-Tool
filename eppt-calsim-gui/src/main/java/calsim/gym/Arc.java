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
 * An arc in the schematic. It typically carries flow
 * in a certain direction. In other words it is a
 * directed arc
 *
 * @author Nicky Sandhu
 * @see Node
 */
public abstract class Arc implements java.io.Serializable
{
	private Node _upNode, _downNode;
	private String _name;
	private int _priority;

	/**
	 * creates an empty arc
	 */
	public Arc(String name)
	{
		this(name, null, null);
	}

	/**
	 * creates an arc between two existing nodes.
	 * If no down node exists then the arc is assumed
	 * to have a universal boundary node that is assumed
	 * to exist around the network.
	 */
	public Arc(String name, Node upNode, Node downNode)
	{
		setName(name);
		setUpstreamNode(upNode);
		setDownstreamNode(downNode);
		_priority = 0; // equivalent to no priority
		// should i check for both nodes being null??
	}

	/**
	 * get upstream node ( back of arrow )
	 */
	public Node getUpstreamNode()
	{
		return _upNode;
	}

	/**
	 * set upstream node
	 */
	public void setUpstreamNode(Node n)
	{
		if(n != null)
		{
			_upNode = n;
		}
		else
		{
			_upNode = GymUtils.UNIVERSAL_BOUNDARY_NODE;
		}
	}

	/**
	 * get downstream node ( arrow point )
	 */
	public Node getDownstreamNode()
	{
		return _downNode;
	}

	/**
	 * set downstream node
	 */
	public void setDownstreamNode(Node n)
	{
		if(n != null)
		{
			_downNode = n;
		}
		else
		{
			_downNode = GymUtils.UNIVERSAL_BOUNDARY_NODE;
		}
	}

	/**
	 * get priority level associated with this arc
	 */
	public int getPriority()
	{
		return _priority;
	}

	/**
	 * set the priority level associated with this arc
	 */
	public void setPriority(int p)
	{
		_priority = p;
	}

	/**
	 * returns the variable represeting the weight of this arc in WRESL
	 */
	public String getWeightVariable()
	{
		return "wt_" + getName();
	}

	/**
	 * get type name abbreviation
	 */
	public abstract String getTypeAbbrev();

	/**
	 * get name of the arc type
	 */
	public abstract String getTypeName();

	/**
	 * @return a string containing the representative
	 * name of the arc.
	 */
	public String getName()
	{
		return _name;
	}

	/**
	 *
	 */
	public void setName(String name)
	{
		_name = name;
	}

	/**
	 * @return the arcs name consisting of its type name
	 * followed by the up node id
	 */
	public String toString()
	{
		return getName();
	}

	/**
	 * dumps information about itself to the standard output
	 */
	public void dump()
	{
		System.out.println("Arc : " + getTypeName());
		System.out.println("Name: " + getName());
		System.out.println("Up node: " + _upNode.getName());
		System.out.println("Down node: " + _downNode.getName());
		System.out.println();
	}
}

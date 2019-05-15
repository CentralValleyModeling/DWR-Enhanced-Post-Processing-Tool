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
package vista.app.schematic;


/**
 *
 */
public class Link
{
	/**
	 *
	 */
	protected Node[] _nodes;
	/**
	 *
	 */
	protected int _id;

	/**
	 *
	 */
	public Link(int id, Node[] nodes)
	{
		_id = id;
		_nodes = nodes;
	}

	/**
	 *
	 */
	public int getId()
	{
		return _id;
	}

	/**
	 *
	 */
	public int getNumberOfNodes()
	{
		return _nodes.length;
	}

	/**
	 *
	 */
	public Node getNode(int localIndex)
	{
		return _nodes[localIndex];
	}

	/**
	 *
	 */
	public String toString()
	{
		StringBuffer buf = new StringBuffer("Link #");
		String eol = System.getProperty("line.separator");
		buf.append(getId()).append(eol);
		return buf.toString();
	}
}
